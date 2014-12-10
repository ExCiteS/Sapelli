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
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Semaphore;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ViewAnimator;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
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
	public void play(final List<PlaybackJob> sequence)
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
				Iterator<PlaybackJob> jobs = sequence.iterator();
				while(running && jobs.hasNext())
					playJob(jobs.next());
			}

		};

		running = true; // TODO this is never actually used?
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
		// Stop playback:
		stop();
		// Release resources:
		if(queuePlayer != null)
		{
			queuePlayer.release();
			queuePlayer = null;
		}
	}

	/**
	 * Play the next item in the playback queue. Precisely, this method executes the following steps:
	 * <ul>
	 * <li>Wait for a new track (acquire {@link AndroidAudioFeedbackController#audioAvailableSem})</li>
	 * <li>Start playing track</li>
	 * <li>Wait for playback to finish (acquire {@link AndroidAudioFeedbackController#playbackCompletedSem})</li>
	 * </ul>
	 */
	private void playJob(PlaybackJob job)
	{
		try
		{
			File audioFile = controller.getFileStorageProvider().getProjectSoundFile(controller.getProject(), job.soundRelativePath);

			// Set the media player's source to the new audio track
			if(FileHelpers.isReadableFile(audioFile))
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
					Log.e(TAG, "Error when trying to change media player data source to file " + job.soundRelativePath, e);
					// Playing failed so completed listener will never fire. Allow playback to continue:
					playbackCompletedSem.release();
				}
			}
			else
			{
				// sleep for a while to indicate "gap" in audio playback:
				Thread.sleep(FFEDBACK_GAP_DURATION_MILISEC);
				// Playing failed so completed listener will never fire. Allow playback to continue:
				playbackCompletedSem.release();
			}
			// animate the view corresponding to the played choice, if necessary:
			animateViewShake(job.viewToAnimate);

			// wait for the media player to finish playing the track
			playbackCompletedSem.acquire(); // TODO shouldn't this happen earlier?

			// wait for the UI thread to finish animating the view
			animationCompletedSem.acquire(); // TODO shouldn't this happen earlier?

		}
		catch(InterruptedException e1)
		{
			// was probably caused by the ChoiceField being exited
			Log.d(TAG, "Playback thread interrupted while waiting for semaphore.");
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

	@SuppressWarnings("unused")
	private void animateViewAlpha(final View toAnimate)
	{
		if(toAnimate != null)
		{
			controller.activity.runOnUiThread(new Runnable()
			{
				@Override
				public void run()
				{
					ViewAnimator.alphaAnimation(toAnimate); // TODO will return immediately! we need an animation completion listener
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
					ViewAnimator.shakeAnimation(context, toAnimate); // TODO will return immediately! we need an animation completion listener
					animationCompletedSem.release();
				}
			});
		}
		else
			animationCompletedSem.release();
	}
	
}