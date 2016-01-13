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

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.LinearLayout;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.media.AudioRecorder;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AVField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;

/**
 * A subclass of AndroidMediaUI which allows for the capture and review of 
 * audio recordings from the device's microphone.
 * 
 * @author mstevens, Michalis Vitos, Julia, benelliott
 *
 */
public class AndroidAudioUI extends AndroidMediaUI<AudioField>
{

	// STATIC -----------------------------------------------------------------
	private static final String TAG = "AndroidAudioUI";
	
	private static final int COLOR_BACKGROUND = Color.BLACK;
	private static final int COLOR_INACTIVE_LEVEL = Color.DKGRAY;
	private static final int COLOR_ACTIVE_LEVEL = Color.rgb(0, 204, 0);
	
	private static final int UPDATE_FREQUENCY_MS = 200;
	private static final int NUM_LEVELS = 50;
	private static final int LEVEL_PADDING = 5;
	private static final float VOLUME_WIDTH_FRACTION = 0.5f; // fraction of the display width that the volume display should span
	
	// DYNAMIC ----------------------------------------------------------------
	private final AudioRecorder audioRecorder;
	
	private VolumeView volumeView;
	private ReviewView audioReviewView;

	public AndroidAudioUI(AudioField field, CollectorController controller, CollectorView collectorUI)
	{
		super(	field,
				controller,
				collectorUI,
				true);	// unblock UI after capture button click to allow other click events (so recording can be stopped)
		audioRecorder = new AudioRecorder();
	}

	@Override
	protected View createCaptureContent(Context context)
	{
		return volumeView = new VolumeView(context);
	}
	
	/**
	 * If not currently recording, will return a "start recording" button. If currently recording, will return a
	 * "stop recording" button.
	 */
	@Override
	protected ImageItem generateCaptureButton(Context context)
	{
		if(!audioRecorder.isRecording())
			// recording hasn't started yet, so present "record" button
			return collectorUI.getImageItemFromProjectFileOrResource(field.getStartRecImageRelativePath(), R.drawable.button_audio_capture);
		else
			// recording started, so present "stop" button instead
			return collectorUI.getImageItemFromProjectFileOrResource(field.getStopRecImageRelativePath(), R.drawable.button_media_stop);
	}
	
	/**
	 * If not already recording, start recording. Else stop recording and attach the media file 
	 * to the field.
	 */
	@Override
	protected void onCapture()
	{
		if(!audioRecorder.isRecording())
			startRecording();
		else
			stopRecording();
	}

	/**
	 * Start writing audio data to the media file, and start displaying the volume level.
	 */
	private void startRecording()
	{
		try
		{
			audioRecorder.start(getNewCaptureFile());
			volumeView.start();
		}
		catch(Exception e)
		{
			handleCaptureError(e);
			return; // !!!
		}
	}

	/**
	 * Stop writing audio data to the media file, and stop displaying the volume level.
	 */
	private void stopRecording()
	{
		try
		{
			volumeView.stop();
			audioRecorder.stop();
		}
		catch(Exception e)
		{
			handleCaptureError(e);
			return; // !!!
		}
		// Attach the media file:
		handleCaptureSuccess();
	}

	@Override
	protected Item<?> getGalleryItem(int index, File attachement)
	{
		return collectorUI.getImageItemFromProjectFileOrResource(field.getRecordingImageRelativePath(), R.drawable.audio_item);
	}

	@Override
	protected View getReviewContent(Context context, File mediaFile)
	{
		if(audioReviewView == null)
			audioReviewView = new ReviewView(context);
		audioReviewView.setAudioFile(mediaFile);
		return audioReviewView;
	}

	@Override
	protected void cancel()
	{
		super.cancel();
		try
		{
			audioRecorder.stop();
		}
		catch(Exception ignore) {}
		
		if(audioReviewView != null)
			audioReviewView.finalise();
	}

	/**
	 * A View that displays a simple visualisation of the current audio amplitude.
	 * 
	 * @author benelliott, mstevens
	 */
	private class VolumeView extends View implements Runnable
	{
				
		private final Handler handler;
	    private final Paint paint;
	    
		private float levelHeight;
		private float levelLeft;
		private float levelRight;		
		
		private volatile int levelsToIlluminate;

		public VolumeView(Context context)
		{
			super(context);
			handler = new Handler(Looper.getMainLooper());
			paint = new Paint();
		}

		@Override
		protected void onSizeChanged(int width, int height, int oldWidth, int oldHeight)
		{
			// (re)calculate level dimensions:
			levelHeight = ((float) height / NUM_LEVELS) - LEVEL_PADDING;
			float levelWidth = VOLUME_WIDTH_FRACTION * width;
			levelLeft = (width - levelWidth) / 2;
			levelRight = (width + levelWidth) / 2;

			super.onSizeChanged(width, height, oldWidth, oldHeight);
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
				canvas.drawRect(levelLeft, levelBottom, levelRight, levelBottom + levelHeight, paint);
			}
		}

		/**
		 * Start visualising the volume.
		 */
		public void start()
		{
			schedule();
		}
		
		private void schedule()
		{
			handler.postDelayed(this, UPDATE_FREQUENCY_MS);
		}
		
		@Override
		public void run()
		{
			if(!audioRecorder.isRecording())
				return;
			float relativeAmplitude = ((float) audioRecorder.getMaxAmplitude()) / (float) AudioRecorder.MAX_AMPLITUDE;
			levelsToIlluminate = Math.min(Math.round(relativeAmplitude * (float) NUM_LEVELS), NUM_LEVELS);
			invalidate(); // ask view to be redrawn
			schedule(); // reschedule for next volume update
		}

		/**
		 * Stop visualising the volume.
		 */
		public void stop()
		{
			handler.removeCallbacks(this);
			levelsToIlluminate = 0;
		}

	}
	
	/**
	 * A subclass of ImageView that provides play/stop functionality when a recording is being reviewed.
	 * 
	 * TODO pause button ({@link AVField#getPausePlaybackImageRelativePath()})
	 * 
	 * @author benelliott, mstevens
	 * @see http://developer.android.com/reference/android/media/MediaPlayer.html#StateDiagram
	 */
	private class ReviewView extends LinearLayout implements MediaPlayer.OnCompletionListener, OnClickListener
	{

		private final View playButton;
		//private final View pauseButton;
		private final View stopButton;

		private MediaPlayer mediaPlayer;
		private boolean paused = false;
		
		public ReviewView(Context context)
		{
			super(context);
						
			// Buttons:
			LinearLayout.LayoutParams buttonParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT);
			playButton = addButtonView(field.getStartPlaybackImageRelativePath(), R.drawable.button_media_play);
			this.addView(playButton, buttonParams);
			stopButton = addButtonView(field.getStopPlaybackImageRelativePath(), R.drawable.button_media_stop);
			this.addView(stopButton, buttonParams);
			
			// Listen for clicks:
			this.setOnClickListener(this);
			
			// Hide stop button at first:
			stopButton.setVisibility(GONE);
		}
		
		private View addButtonView(String imgPath, int drawableID)
		{
			ImageItem item = collectorUI.getImageItemFromProjectFileOrResource(imgPath, drawableID).setBackgroundColor(fieldBackgroundColor);
			if(item.isUsingResource())
				item.setPaddingDip(PLAYBACK_BUTTON_PADDING_DIP); // the built-in play/stop SVGs don't look right with the default padding
			return item.getView(getContext());
		}
		
		public void setAudioFile(File audioFile)
		{
			try
			{	// Set-up mediaPlayer:
				mediaPlayer = new MediaPlayer();
				mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
				mediaPlayer.setOnCompletionListener(this);
				mediaPlayer.setDataSource(getContext(), Uri.fromFile(audioFile));
				paused = false;
			}
			catch(Exception e)
			{
				Log.e(TAG, "Could not set audio file for playback.", e);
			}
		}
		
		/**
		 * Note: no animation because image will change immediately
		 * 
		 * @param v
		 */
		@Override
		public void onClick(View v)
		{
			if(mediaPlayer == null)
				return; // just in case
			controller.blockUI();
			if(!mediaPlayer.isPlaying())
				// if not playing, then start playing audio
				playAudio();
			else
				// if already playing, then stop audio
				stopAudio();
			controller.unblockUI();
		}
		
		/*
		 * Start playing audio and display the "stop" button.
		 */
		private void playAudio()
		{
			try
			{
				if(!paused)
				{
					mediaPlayer.prepare();
					mediaPlayer.seekTo(0);
				}
				mediaPlayer.start();
				paused = false;
			}
			catch(Exception e)
			{
				Log.e(TAG, "Could not play audio file.", e);
				return;
			}
			
			// present the stop button to the user:
			playButton.setVisibility(GONE);
			stopButton.setVisibility(VISIBLE);
		}

		/**
		 * Stop playing audio and display the "play" button.
		 */
		public void stopAudio()
		{
			// stop the playback:
			try
			{
				mediaPlayer.stop();
				paused = false;
			}
			catch(Exception ignore) {}
			
			// present the play button to the user:
			stopButton.setVisibility(GONE);
			playButton.setVisibility(VISIBLE);
		}
		
//		/**
//		 * Pause playing audio and display the "play" button.
//		 */
//		public void pauseAudio()
//		{
//			// stop the playback:
//			try
//			{
//				mediaPlayer.pause();
//				paused = true;
//			}
//			catch(Exception ignore) {}
//			
//			// present the play button to the user:
//			stopButton.setVisibility(GONE);
//			playButton.setVisibility(VISIBLE);
//		}

		/**
		 * When the media player finishes playing the recording, set it to the "stopped" state
		 * so the user can play it again.
		 */
		@Override
		public void onCompletion(MediaPlayer mp)
		{
			stopAudio();
		}
		
		public void finalise()
		{
			try
			{
				mediaPlayer.stop();
				mediaPlayer.release();
				mediaPlayer = null;
			}
			catch(Exception ignore) {}
		}
		
	}

}
