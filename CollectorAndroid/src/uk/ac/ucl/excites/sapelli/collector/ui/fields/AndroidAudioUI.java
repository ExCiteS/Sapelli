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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;
import java.io.IOException;
import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.media.AudioRecorder;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.AudioItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.net.Uri;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceHolder.Callback;
import android.view.SurfaceView;
import android.view.View;
import android.widget.LinearLayout;

/**
 * A subclass of AndroidMediaUI which allows for the capture and review of 
 * audio recordings from the device's microphone.
 * 
 * @author mstevens, Michalis Vitos, Julia, benelliott
 *
 */
public class AndroidAudioUI extends AndroidMediaUI<AudioField>
{

	private boolean recording = false;

	private static final String TAG = "AndroidAudioUI";

	private AudioRecorder audioRecorder;
	private AudioReviewView audioReviewView;
	private VolumeDisplaySurfaceView volumeDisplay;

	public AndroidAudioUI(AudioField field, CollectorController controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}

	/**
	 * Start writing audio data to the media file, and start displaying the volume level.
	 * @return whether or not recording was started successfully.
	 */
	private boolean startRecording()
	{
		try
		{
			audioRecorder = new AudioRecorder(captureFile);
			audioRecorder.start();
			volumeDisplay.start();
		}
		catch(IOException ioe)
		{
			Log.e(TAG, "Could not get audio file.", ioe);
			if (isValid(controller.getCurrentRecord()))
				controller.goForward(false);
			else
				controller.goToCurrent(LeaveRule.UNCONDITIONAL_NO_STORAGE);
			return false;
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not start audio recording.", e);
			attachMedia(null); //TODO remove? only functionality is to log that attaching failed
			if (isValid(controller.getCurrentRecord()))
				controller.goForward(false);
			else
				controller.goToCurrent(LeaveRule.UNCONDITIONAL_NO_STORAGE);
			return false; // !!!
		}
		return true;		
	}

	/**
	 * Stop writing audio data to the media file, and stop displaying the volume level.
	 */
	private void stopRecording()
	{
		try
		{
			volumeDisplay.stop();
			audioRecorder.stop();
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error on stopping audio recording.", e);
		}
		finally
		{
			audioRecorder = null;
		}
	}

	/**
	 * If not already recording, start recording. Else stop recording and attach the media file 
	 * to the field.
	 */
	@Override
	protected boolean onCapture()
	{
		if(!recording)
		{
			// start recording
			minimiseCaptureButton(); // show volume levels while recording
			captureFile = field.getNewAttachmentFile(controller.getFileStorageProvider(), controller.getCurrentRecord());
			startRecording();
			recording = true;
		}
		else
		{
			// stop recording
			stopRecording();
			// a capture has been made so show it for review:
			attachMedia(captureFile);
			recording = false;
			if(field.isShowReview())
				controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
			else
				controller.goForward(true);
		}
		// always allow other click events after this completes (so recording can be stopped by pressing again):
		return true;
	}

	@Override
	protected void onDiscard()
	{
		if(audioReviewView != null)
			audioReviewView.finalise();
	}

	/**
	 * If not currently recording, will return a "start recording" button. If currently recording, will return a
	 * "stop recording" button.
	 */
	@Override
	protected ImageItem generateCaptureButton(Context context)
	{
		ImageItem captureButton = null;
		if(!recording)
		{
			// recording hasn't started yet, so present "record" button
			File captureImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(), field.getStartRecImageRelativePath());
			if(FileHelpers.isReadableFile(captureImgFile))
				// use a custom audio capture image if available
				captureButton = new FileImageItem(captureImgFile);
			else
				// otherwise just use the default resource
				captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_audio_capture_svg);
		}
		else
		{
			// recording started, so present "stop" button instead
			File stopImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(), field.getStopRecImageRelativePath());
			if(FileHelpers.isReadableFile(stopImgFile))
				captureButton = new FileImageItem(stopImgFile);
			else
				captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_stop_audio_svg);
		}
		captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return captureButton;
	}

	@Override
	protected Item getItemFromFile(File file)
	{
		return new AudioItem(file);
	}

	@Override
	protected View getCaptureContent(Context context)
	{
		volumeDisplay = new VolumeDisplaySurfaceView(context);
		return volumeDisplay;
	}

	@Override
	protected View getReviewContent(Context context, File mediaFile)
	{
		audioReviewView = new AudioReviewView(context, mediaFile);
		return audioReviewView;
	}

	/**
	 * Requests that the capture button be maximised when the capture UI is entered.
	 */
	@Override
	protected boolean isMaximiseCaptureButton()
	{
		return true;
	}

	@Override
	protected void cancel()
	{
		super.cancel();
		if(audioRecorder != null)
			stopRecording();

		recording = false;
		if(audioReviewView != null)
			audioReviewView.finalise();

		audioReviewView = null;
		volumeDisplay = null;
	}

	/**
	 * A subclass of ImageView that provides play/stop functionality when a recording is being reviewed.
	 * 
	 * @author benelliott
	 */
	private class AudioReviewView extends LinearLayout implements MediaPlayer.OnCompletionListener
	{

		private MediaPlayer mediaPlayer = new MediaPlayer();
		private Runnable buttonAction;
		private final View playButton;
		private final View stopButton;
		private boolean playing = false;

		public AudioReviewView(Context context, File audioFile)
		{
			super(context);
			try
			{
				mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
				mediaPlayer.setDataSource(context, Uri.fromFile(audioFile));
				mediaPlayer.setOnCompletionListener(this);
			}
			catch(IOException e)
			{
				Log.e(TAG, "Could not play audio file.");
				e.printStackTrace();
			}
			File playImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(), field.getPlayAudioImageRelativePath());
			if(FileHelpers.isReadableFile(playImgFile))
				playButton = new FileImageItem(playImgFile).getView(context);
			else
				playButton = new ResourceImageItem(context.getResources(), R.drawable.button_play_audio_svg).getView(context);
			// playAudioButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
			playButton.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT));

			File stopImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(), field.getStopAudioImageRelativePath());
			if(FileHelpers.isReadableFile(stopImgFile))
				stopButton = new FileImageItem(stopImgFile).getView(context);
			else
				stopButton = new ResourceImageItem(context.getResources(), R.drawable.button_stop_audio_svg).getView(context);
			// stopAudioButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
			stopButton.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT));

			buttonAction = new Runnable()
			{
				public void run()
				{
					if(!playing)
					{
						// if not playing, then start playing audio
						playAudio();
						playing = true;
					}
					else
					{
						// if already playing, then stop audio
						stopAudio();
						playing = false;
					}
					controller.unblockUI();
				}
			};

			this.setOnClickListener(new OnClickListener()
			{

				@Override
				public void onClick(View v)
				{
					controller.clickView(v, buttonAction);
				}

			});
			// show play button first
			this.addView(playButton);

		}
		/*
		 * Start playing audio and display the "stop" button.
		 */
		private void playAudio()
		{
			// play the audio:
			try
			{
				mediaPlayer.prepare();
				mediaPlayer.start();
			}
			catch(IOException e)
			{
				Log.e(TAG, "Could not play audio file.");
				e.printStackTrace();
			}
			// present the play button to the user:

			removeView(playButton);
			addView(stopButton);
		}

		/**
		 * Stop playing audio and display the "play" button.
		 */
		private void stopAudio()
		{
			// stop the audio:
			mediaPlayer.stop();
			// present the play button to the user:
			removeView(stopButton);
			addView(playButton);
		}

		/**
		 * Release the media player.
		 */
		private void finalise()
		{
			if(mediaPlayer != null)
			{
				mediaPlayer.reset();
				mediaPlayer.release();
			}
			mediaPlayer = null;
			playing = false;
		}

		/**
		 * When the media player finishes playing the recording, set it to the "stopped" state
		 * so the user can play it again.
		 */
		@Override
		public void onCompletion(MediaPlayer mp)
		{
			// called when the media player finishes playing its media file
			// go from PlaybackCompleted to Stopped
			stopAudio();
			playing = false;
		}
	}

	/**
	 * A SurfaceView that displays a simple visualisation of the current audio amplitude,
	 * to be used when recording has been started.
	 * 
	 * @author benelliott
	 */
	private class VolumeDisplaySurfaceView extends SurfaceView
	{
		
		private static final int COLOR_BACKGROUND = Color.BLACK;
		private static final int COLOR_INACTIVE_LEVEL = Color.DKGRAY;
		private final int COLOR_ACTIVE_LEVEL = Color.rgb(0, 204, 0);
		private static final int UPDATE_FREQUENCY_MILLISEC = 200;
		private static final double MAX_AMPLITUDE = 30000D; // TODO might need tweaking
		private static final int NUM_LEVELS = 50;
		private static final int LEVEL_PADDING = 5;
		private static final float VOLUME_WIDTH_FRACTION = 0.5f; // fraction of the display width that the volume display should span
		Timer timer;
		private Paint paint;
		private int amplitude;
		private int levelsToIlluminate;
		private float levelWidth;
		private float levelHeight;
		private float levelLeft;
		private float levelRight;

		public VolumeDisplaySurfaceView(Context context)
		{
			super(context);
			paint = new Paint();
			getHolder().addCallback(new Callback()
			{

				@Override
				public void surfaceCreated(SurfaceHolder holder)
				{
				}

				@SuppressLint("WrongCall")
				@Override
				public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
				{
					// calculate level dimensions:
					levelHeight = ((float) getHeight() / NUM_LEVELS) - LEVEL_PADDING;
					levelWidth = VOLUME_WIDTH_FRACTION * getWidth();
					levelLeft = (getWidth() - levelWidth) / 2;
					levelRight = (getWidth() + levelWidth) / 2;

					Canvas c = holder.lockCanvas(null);
					onDraw(c);
					holder.unlockCanvasAndPost(c);
				}

				@Override
				public void surfaceDestroyed(SurfaceHolder holder)
				{
					// nothing to do
				}
			});
		}

		/**
		 * Illuminate the number of levels specified by levelsToIlluminate.
		 */
		@Override
		protected void onDraw(Canvas canvas)
		{
			// wipe everything off:
			canvas.drawColor(COLOR_BACKGROUND);
			// draw the levels:
			paint.setColor(COLOR_ACTIVE_LEVEL);

			for(int i = 0; i < NUM_LEVELS; i++)
			{

				if(i == levelsToIlluminate)
					paint.setColor(COLOR_INACTIVE_LEVEL);

				float levelBottom = getHeight() - i * (levelHeight + LEVEL_PADDING); // remember top-left is (0,0)

				canvas.drawRect(levelLeft, levelBottom + levelHeight, levelRight, levelBottom, paint);
			}

		}

		/**
		 * Start the TimerTask to visualise the volume.
		 */
		private void start()
		{
			timer = new Timer();
			timer.schedule(new VolumeDisplayTask(), 0, UPDATE_FREQUENCY_MILLISEC);
		}

		/**
		 * Stop the TimerTask that visualises the volume.
		 */
		private void stop()
		{
			timer.cancel();
			timer = null;
		}

		/**
		 * A TimerTask that periodically checks the current audio amplitude, calculates
		 * the number of "levels" that should be illuminated, and then draws them to the screen.
		 * 
		 * @author benelliott
		 */
		private class VolumeDisplayTask extends TimerTask
		{

			@SuppressLint("WrongCall")
			@Override
			public void run()
			{
				amplitude = audioRecorder.getMaxAmplitude();
				// see how loud it currently is relative to MAX_AMPLITUDE, then multiply that fraction
				// by the number of available levels:
				levelsToIlluminate = (int) (((double) amplitude / MAX_AMPLITUDE) * (double) NUM_LEVELS);
				if(levelsToIlluminate > NUM_LEVELS)
					levelsToIlluminate = NUM_LEVELS;
				Canvas c = null;
				try
				{
					c = getHolder().lockCanvas();
					synchronized(getHolder())
					{
						if(c != null)
							onDraw(c);
					}
				}
				finally
				{
					if(c != null)
					{
						getHolder().unlockCanvasAndPost(c);
					}
				}
			}

		}
	}
}
