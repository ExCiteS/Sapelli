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

		// Initialise semaphores with 0 permits (release() must be called to issue permit):
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
		running = true;
		playbackThread.start();
	}

	@Override
	public void stop()
	{
		Log.d(TAG, "Stopping playback...");
		running = false;
		// Interrupt playback thread since it is probably blocked on a semaphore:
		if(playbackThread != null)
		{
			playbackThread.interrupt();
			try
			{	// Wait for playback thread to die before continuing:
				playbackThread.join();
			}
			catch(InterruptedException e)
			{
				Log.e(TAG, "Main thread interrupted while waiting for playback thread to join.");
			}
		}
		// Stop currently playing track:
		if(queuePlayer != null && queuePlayer.isPlaying())
			queuePlayer.stop();
		// Nullify thread so it must be re-initialised:
		playbackThread = null;
		// Destroy semaphores so they aren't reused for later jobs/sequences:
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
					// Start audio playback:
					queuePlayer.start();
					
					// Animate the view, if necessary:
					if(job.viewToAnimate != null)
					{
						Runnable releaseSem = new Runnable()
						{
							@Override
							public void run()
							{
								animationCompletedSem.release();
							}
						};
						switch(job.animation)
						{
							case ANIMATION_ALPHA :
								ViewAnimator.Alpha(job.viewToAnimate, null, releaseSem);
								break;
							case ANIMATION_SHAKE :
								ViewAnimator.Shake(job.viewToAnimate, null, releaseSem);
								break;
							default :
								throw new Exception("Unknown animation: " + job.animation);
						}
					}
					else
						animationCompletedSem.release();
				}
				catch(InterruptedException ie)
				{	// re-throw so InterruptedExceptions are not caught by the Exception catch below and instead by the outer InterruptionException catch:
					throw ie;
				}
				catch(Exception e)
				{
					Log.e(TAG, "Error when trying to change media player data source to file " + job.soundRelativePath, e);
					// Playing failed so completed listener will never fire. Allow playback to continue:
					playbackCompletedSem.release();
					animationCompletedSem.release();
				}
			}
			else
			{
				// Sleep for a while to indicate "gap" in audio playback:
				Thread.sleep(FFEDBACK_GAP_DURATION_MILISEC);
				// Allow playback to continue:
				playbackCompletedSem.release();
				animationCompletedSem.release();
			}

			// Wait for the media player to finish playing the track
			playbackCompletedSem.acquire();

			// Wait for the UI thread to finish animating the view
			animationCompletedSem.acquire();

		}
		catch(InterruptedException ie)
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
	
}