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

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.media.CameraController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.VideoField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.VideoItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.content.Context;
import android.graphics.Bitmap;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.media.ThumbnailUtils;
import android.net.Uri;
import android.provider.MediaStore;
import android.util.Log;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnTouchListener;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.LinearLayout;
import android.widget.VideoView;

/**
 * A subclass of AndroidMediaUI which allows for the capture and review of videos from the device's camera.
 * 
 * @author mstevens, Michalis Vitos, benelliott
 *
 */
public class AndroidVideoUI extends AndroidMediaUI<VideoField> implements OnCompletionListener
{

	static private final String TAG = "AndroidVideoUI";

	// Camera & image data:
	private CameraController cameraController;
	private SurfaceView captureSurface;
	private VideoView playbackView;
	private volatile Boolean recording = false;
	private int playbackPosition = 0;

	public AndroidVideoUI(VideoField field, CollectorController controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}

	@SuppressWarnings("deprecation")
	@Override
	protected View getCaptureContent(Context context)
	{
		if(cameraController == null)
		{
			// Set up cameraController:
			// Camera controller & camera selection:
			cameraController = new CameraController(field.isUseFrontFacingCamera());
			if(!cameraController.foundCamera())
			{ // no camera found, try the other one:
				cameraController.findCamera(!field.isUseFrontFacingCamera());
				if(!cameraController.foundCamera())
				{ // still no camera, this device does not seem to have one:
					attachMedia(null);
					if(isValid(controller.getCurrentRecord()))
						controller.goForward(false);
					else
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_NO_STORAGE);
					return null;
				}
			}
		}
		// Create the surface for previewing the camera:
		captureSurface = new SurfaceView(context);

		// Set-up surface holder:
		SurfaceHolder holder = captureSurface.getHolder();
		holder.addCallback(cameraController);
		holder.setKeepScreenOn(true);
		// !!! Deprecated but cameraController preview crashes without it (at least on the XCover/Gingerbread):
		holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);

		cameraController.startPreview();

		return captureSurface;
	}

	@Override
	protected boolean onCapture()
	{
		synchronized(recording)
		{
			if(!recording)
			{
				// start recording
				captureFile = field.getNewAttachmentFile(controller.getFileStorageProvider(), controller.getCurrentRecord());
				cameraController.startVideoCapture(captureFile);
				recording = true;
			}
			else
			{
				// stop recording
				cameraController.stopVideoCapture();
				// a capture has been made so show it for review:
				attachMedia(captureFile);
				recording = false;
				if(field.isShowReview())
					controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
				else
					controller.goForward(true);
			}
		}
		// always allow other click events after this completes (so recording can be stopped by pressing again):
		return true;
	}

	@Override
	protected void onDiscard()
	{
		// nothing to do
	}

	@Override
	protected View getReviewContent(Context context, File mediaFile)
	{
		LinearLayout reviewLayout = new LinearLayout(context);
		reviewLayout.setGravity(Gravity.CENTER);
		// instantiate the thumbnail that is shown before the video is started:
		final ImageView thumbnailView = new ImageView(context);
		// create thumbnail from video file:
		Bitmap thumbnail = ThumbnailUtils.createVideoThumbnail(mediaFile.getAbsolutePath(), MediaStore.Images.Thumbnails.FULL_SCREEN_KIND);
		thumbnailView.setScaleType(ScaleType.FIT_CENTER);
		thumbnailView.setImageBitmap(thumbnail);

		// instantiate the video view that plays the captured video:
		playbackView = new VideoView(context);
		playbackView.setOnCompletionListener(this);
		playbackView.setVideoURI(Uri.fromFile(mediaFile));
		// don't show the video view straight away - only once the thumbnail is clicked:
		playbackView.setVisibility(View.GONE);

		// layout params for the thumbnail and the video view are the same:
		LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.MATCH_PARENT);
		reviewLayout.addView(thumbnailView, params);
		reviewLayout.addView(playbackView, params);

		// Set the video view to play or pause the video when it is touched:
		playbackView.setOnTouchListener(new OnTouchListener()
		{
			@Override
			public boolean onTouch(View v, MotionEvent ev)
			{
				controller.blockUI();
				if(ev.getAction() == MotionEvent.ACTION_UP)
				{
					// only perform action when finger is lifted off screen
					if(playbackView.isPlaying())
					{
						// if playing, pause
						Log.d(TAG, "Pausing video...");
						playbackPosition = playbackView.getCurrentPosition();
						playbackView.pause();
					}

					else
					{
						// if not playing, play
						Log.d(TAG, "Playing video...");
						playbackView.seekTo(playbackPosition);
						playbackView.start();
					}
				}
				controller.unblockUI();
				return true;
			}
		});

		// replace the thumbnail with the video when the thumbnail is clicked:
		thumbnailView.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				controller.blockUI();
				thumbnailView.setVisibility(View.GONE);
				playbackView.setVisibility(View.VISIBLE);
				playbackView.start();
				controller.unblockUI();
			}
		});

		return reviewLayout;
	}

	@Override
	public void onCompletion(MediaPlayer mp)
	{
		// playback has finished so go back to start
		playbackPosition = 0;
	}

	/**
	 * If not currently recording, will return a "start recording" button. If currently recording, will return a "stop recording" button.
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
				// use a custom video capture image if available
				captureButton = new FileImageItem(captureImgFile);
			else
				// otherwise just use the default resource
				captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_video_capture_svg);
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
		return new VideoItem(file);
	}

	@Override
	protected void cancel()
	{
		super.cancel();
		synchronized(recording)
		{
			recording = false;
			if(cameraController != null)
			{
				cameraController.close();
			}
		}
		cameraController = null;

		playbackView = null;
	}
}