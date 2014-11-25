/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.media;

import java.io.File;
import java.util.List;
import java.util.concurrent.Semaphore;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ViewAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import android.content.Context;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.util.Log;
import android.view.View;

/**
 * Class which plays the audio descriptions of choice fields.
 * 
 * @author Michalis Vitos, benelliott
 *
 */
public class AndroidAudioFeedbackController extends AudioFeedbackController<View> implements OnCompletionListener
{

	private static final String TAG = "AudioFeedbackController";
	private static final int FFEDBACK_GAP_DURATION_MILISEC = 1000; // time (in milisec) to pause if a piece of audio is not available

	private CollectorController controller;
	private Context context;

	private Thread playbackThread;
	private volatile boolean running = false;
	private MediaPlayer queuePlayer;
	private Semaphore playbackCompletedSem; // semaphore used to notify when the media player has finished playing the current track
	private Semaphore animationCompletedSem; // semaphore used to notify when the UI thread has finished animating the view

	public AndroidAudioFeedbackController(CollectorController controller)
	{
		this.controller = controller;
		this.context = controller.activity.getApplicationContext();
	}

	@Override
	public void play(final List<AudioFeedbackController<View>.PlaybackJob> sequence)
	{
		stop(); // stop any already-playing playlists prematurely
		Log.d(TAG, "Starting playback...");

		playbackCompletedSem = new Semaphore(0);
		animationCompletedSem = new Semaphore(0);

		playbackThread = new Thread()
		{

			@Override
			public void run()
			{
				while(running && !sequence.isEmpty())
				{
					playNextQueueItem(sequence);
				}
			}

		};

		running = true;
		playbackThread.start();
	}

	@Override
	public void stop()
	{
		Log.d(TAG, "Stopping playback...");
		running = false;
		// stop currently playing track:
		if(queuePlayer != null && queuePlayer.isPlaying())
			queuePlayer.stop();
		// interrupt playback thread since it is probably blocked on a semaphore:
		if(playbackThread != null)
		{
			playbackThread.interrupt();
			try
			{
				// wait for playback thread to die before continuing:
				playbackThread.join();
			}
			catch(InterruptedException e)
			{
				Log.e(TAG, "Main thread interrupted while waiting for playback thread to join.");
			}
		}
		// nullify thread so it must be re-initialised:
		playbackThread = null;
		// destroy semaphore so it isn't reused by later ChoiceFields:a
		animationCompletedSem = null;
		playbackCompletedSem = null;
	}

	@Override
	public void destroy()
	{
		stop();

		if(queuePlayer != null)
		{
			queuePlayer.stop();
			queuePlayer.release();
			queuePlayer = null;
		}
	}

	/**
	 * Play the next item in the queue, and once it has finished, remove it from the queue and delete the file if necessary (if the file was a temporary file
	 * used to store synthesised text). Precisely, this method executes the following steps:
	 * <ul>
	 * <li>Wait for a new track (acquire {@link AndroidAudioFeedbackController#audioAvailableSem})</li>
	 * <li>Start playing track</li>
	 * <li>Wait for playback to finish (acquire {@link AndroidAudioFeedbackController#playbackCompletedSem})</li>
	 * <li>Remove the just-played item from the queue</li>
	 * </ul>
	 */
	private void playNextQueueItem(List<PlaybackJob> mediaQueue)
	{
		try
		{

			PlaybackJob currentTrack = mediaQueue.remove(0);
			File audioFile = controller.getFileStorageProvider().getProjectSoundFile(controller.getProject(), currentTrack.soundRelativePath);

			// set the media player's source to the new audio track
			if(audioFile != null)
			{
				try
				{
					if(queuePlayer == null)
					{
						queuePlayer = MediaPlayer.create(context, Uri.fromFile(audioFile));
						queuePlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
						queuePlayer.setOnCompletionListener(this);
					}
					else
					{
						queuePlayer.reset(); // reset to idle state
						queuePlayer.setDataSource(context, Uri.fromFile(audioFile));
						queuePlayer.prepare();
					}
					// start it playing:
					queuePlayer.start();

				}
				catch(Exception e)
				{
					Log.e(TAG, "Error when trying to change media player data source to file " + currentTrack.soundRelativePath, e);
					// Playing failed so completed listener will never fire. Allow file to be deleted and playback to continue:
					playbackCompletedSem.release();
				}
			}
			else
			{
				// sleep for a while to indicate "gap" in audio playback:
				Thread.sleep(FFEDBACK_GAP_DURATION_MILISEC);
				// Playing failed so completed listener will never fire. Allow file to be deleted and playback to continue:
				playbackCompletedSem.release();
			}
			// animate the view corresponding to the played choice, if necessary:
			animateViewShake(currentTrack.viewToAnimate);

			// wait for the media player to finish playing the track
			playbackCompletedSem.acquire();

			// wait for the UI thread to finish animating the view
			animationCompletedSem.acquire();

		}
		catch(InterruptedException e1)
		{
			// was probably caused by the ChoiceField being exited
			Log.d(TAG, "Playback thread interrupted while waiting for semaphore.");
		}
	}

	/**
	 * Speaks aloud the description of a ControlItem and animates the ControlItem. The description used can be either audio file or text that uses Android's TTS
	 * (Text-To-Speech) engine to read it aloud which is defined in the XML of a project.
	 * 
	 * @param controlItem
	 * @param controlView
	 */
	public void playAnswer(Item controlItem, View controlView)
	{
		// Check whether AudioFeedback is supported for the current form
		AudioFeedback audioFeedback = controller.getCurrentForm().getAudioFeedback();

		if(audioFeedback != null)
		{
			switch(audioFeedback)
			{
			case LONG_CLICK:
			case SEQUENTIAL:

				// TODO: decide TTS / MediaPlayer

				// If the choice has an audio, pass that audio to the Media Player
				if(false)
					return;
				else
					// Enable TTS Audio Feedback
					// textToVoice(controlItem.getDescription()); TODO
					break;

			case NONE:
				return;
			}

			// Apply an alpha animation to the long pressed view
			// animateViewAlpha(controlView);
		}
	}

	/**
	 * When the media player completes, notify the player thread by releasing the playback semaphore.
	 */
	@Override
	public void onCompletion(MediaPlayer mp)
	{
		playbackCompletedSem.release();
	}

	private void animateViewAlpha(final View toAnimate)
	{
		if(toAnimate != null)
		{
			controller.activity.runOnUiThread(new Runnable()
			{
				@Override
				public void run()
				{
					ViewAnimator.alphaAnimation(toAnimate);
					animationCompletedSem.release();
				}
			});
		}
		else
			animationCompletedSem.release();
	}

	private void animateViewShake(final View toAnimate)
	{
		if(toAnimate != null)
		{
			controller.activity.runOnUiThread(new Runnable()
			{
				@Override
				public void run()
				{
					ViewAnimator.shakeAnimation(context, toAnimate);
					animationCompletedSem.release();
				}
			});
		}
		else
			animationCompletedSem.release();
	}
}