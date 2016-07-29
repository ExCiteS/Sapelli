/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.drawable.BitmapDrawable;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.media.ThumbnailUtils;
import android.net.Uri;
import android.provider.MediaStore;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnTouchListener;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.VideoView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.AndroidCollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.MediaFile;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AVField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField.FlashMode;
import uk.ac.ucl.excites.sapelli.collector.model.fields.VideoField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.DrawableItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;

/**
 * A subclass of AndroidMediaUI which allows for the capture and review of videos from the device's camera.
 * 
 * NOTE: Samsung decided not to bother to enable portrait photo/video capture in the xCover 1 kernel, so captures may display incorrectly on that model.
 * -- See http://stackoverflow.com/questions/19176038
 * 
 * @author mstevens, Michalis Vitos, benelliott
 */
public class AndroidVideoUI extends AndroidCameraUI<VideoField>
{
	
	static protected final String TAG = "AndroidVideoUI";
	
	private ReviewView reviewView;

	public AndroidVideoUI(VideoField field, AndroidCollectorController controller, CollectorView collectorUI)
	{
		super(	field,
				controller,
				collectorUI,
				true);	// unblock UI after capture button click to allow other click events (so recording can be stopped)
	}

	@Override
	protected View createCaptureContent(Context context)
	{
		return getCaptureContent(context, true, field.isUseFrontFacingCamera(), FlashMode.OFF); // TODO expose flash mode in XML for <Video>?
	}
	
	/**
	 * If not currently recording, will return a "start recording" button. If currently recording, will return a "stop recording" button.
	 */
	@Override
	protected ImageItem generateCaptureButton(Context context)
	{
		if(cameraController == null || !cameraController.isRecording())
			// recording hasn't started yet, so present "record" button
			return collectorUI.getImageItemFromProjectFileOrResource(field.getStartRecImageRelativePath(), R.drawable.button_video_capture);
		else
			// recording started, so present "stop" button instead
			return collectorUI.getImageItemFromProjectFileOrResource(field.getStopRecImageRelativePath(), R.drawable.button_media_stop);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.AndroidCameraUI#doCapture()
	 */
	@Override
	protected void doCapture()
	{
		if(!cameraController.isRecording())
		{	// start recording
			try
			{
				cameraController.startVideoCapture(getNewCaptureFile().file);
			}
			catch(Exception e)
			{
				handleCaptureError(e);
			}
		}
		else
		{	// stop recording
			cameraController.stopVideoCapture();
			// a capture has been made so show it for review:
			handleCaptureSuccess();
		}
	}
	
	@Override
	protected View getReviewContent(Context context, MediaFile mediaFile)
	{
		if(reviewView == null)
			reviewView = new ReviewView(context);
		reviewView.update(mediaFile.file);
		return reviewView;
	}

	@Override
	protected Item<?> getGalleryItem(int index, MediaFile attachment)
	{
		// Create thumbnail from video file:
		Bitmap thumbnail = ThumbnailUtils.createVideoThumbnail(attachment.file.getAbsolutePath(), MediaStore.Images.Thumbnails.MINI_KIND);
		return new DrawableItem(index, new BitmapDrawable(collectorUI.getResources(), thumbnail));
	}

	@Override
	protected int getCameraErrorStringId(boolean fatal)
	{
		return fatal ? R.string.videoCameraErrorFatal : R.string.videoCameraErrorSkip;
	}
	
	@Override
	protected void cancel()
	{
		super.cancel();		
		if(isInReviewItemMode(false) && reviewView != null)
			reviewView.playbackView.stopPlayback();
	}
	
	/**
	 * TODO stop button ({@link AVField#getStopPlaybackImageRelativePath()})
	 * TODO pause button ({@link AVField#getPausePlaybackImageRelativePath()})
	 * 
	 * @author benelliott, mstevens
	 */
	private class ReviewView extends LinearLayout implements OnCompletionListener, OnTouchListener, OnClickListener
	{
		
		private final View playButton;
		//private final View pauseButton;
		//private final View stopButton;
		
		final RelativeLayout thumbnailView;
		final ImageView imgThumbnail;
		final VideoView playbackView;
		
		public ReviewView(Context context)
		{
			super(context);
			
			setGravity(Gravity.CENTER);
			
			// LayoutParams for child views:
			LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.MATCH_PARENT);
			
			// ImageView that displays a thumbnail of the video before playback is started:
			RelativeLayout.LayoutParams thumbnailParams = new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT, RelativeLayout.LayoutParams.MATCH_PARENT);
			thumbnailView = new RelativeLayout(context);
			imgThumbnail = new ImageView(context);
			imgThumbnail.setScaleType(ScaleType.FIT_CENTER);
			thumbnailView.addView(imgThumbnail, thumbnailParams);
			playButton = addButtonView(field.getStartPlaybackImageRelativePath(), R.drawable.button_media_play);
			playButton.setBackgroundColor(Color.TRANSPARENT);
			thumbnailView.addView(playButton, thumbnailParams);
			thumbnailView.setOnClickListener(this);
			addView(thumbnailView, params);
			
			// VideoView that plays the captured video:
			playbackView = new VideoView(context);
			playbackView.setOnCompletionListener(this);
			playbackView.setOnTouchListener(this);
			addView(playbackView, params);
		}
		
		private View addButtonView(String imgPath, int drawableID)
		{
			ImageItem item = collectorUI.getImageItemFromProjectFileOrResource(imgPath, drawableID).setBackgroundColor(fieldBackgroundColor);
			if(item.isUsingResource())
				item.setPaddingDip(PLAYBACK_BUTTON_PADDING_DIP); // the built-in play/stop SVGs don't look right with the default padding
			return item.getView(getContext());
		}
		
		public void update(File videoFile)
		{
			// create thumbnail from video file:
			imgThumbnail.setImageBitmap(ThumbnailUtils.createVideoThumbnail(videoFile.getAbsolutePath(), MediaStore.Images.Thumbnails.FULL_SCREEN_KIND));
			thumbnailView.setVisibility(View.VISIBLE);
			
			playbackView.setVideoURI(Uri.fromFile(videoFile));
			playbackView.setVisibility(View.GONE); // don't show the video view straight away - only once the thumbnail is clicked
		}
		
		/**
		 * Makes the video view to play or pause the video when it is touched
		 * 
		 * @see android.view.View.OnTouchListener#onTouch(android.view.View, android.view.MotionEvent)
		 */
		@SuppressLint("ClickableViewAccessibility")
		public boolean onTouch(View v, MotionEvent ev)
		{
			if(v != playbackView)
				return false; // just in case			
			if(ev.getAction() == MotionEvent.ACTION_UP) // only perform action when finger is lifted off screen
			{
				controller.blockUI();
				//Log.d(TAG, "Video playing: " + playbackView.isPlaying());
				if(playbackView.isPlaying())
				{	// if playing, pause
					playbackView.pause();
				}
				else
				{	// if not playing, play (will resume if paused)
					playbackView.start();
				}
				controller.unblockUI();
			}
			return true;
		}
		
		/**
		 * Replace the thumbnail with the video when the thumbnail is clicked
		 * 
		 * @see android.view.View.OnClickListener#onClick(android.view.View)
		 */
		@Override
		public void onClick(View v)
		{
			if(v != thumbnailView)
				return; // just in case
			controller.blockUI();
			thumbnailView.setVisibility(View.GONE);
			playbackView.setVisibility(View.VISIBLE);
			playbackView.start();
			controller.unblockUI();
		}
		
		@Override
		public void onCompletion(MediaPlayer mp)
		{
			thumbnailView.setVisibility(View.VISIBLE);
			playbackView.setVisibility(View.GONE);
		}
		
	}
	
}