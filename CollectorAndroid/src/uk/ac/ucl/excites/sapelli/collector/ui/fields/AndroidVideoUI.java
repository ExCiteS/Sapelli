package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.Controller;
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
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.util.Log;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.VideoView;

/**
 * A subclass of AndroidMediaUI which allows for the capture and 
 * review of videos from the device's camera.
 * 
 * @author mstevens, Michalis Vitos, benelliott
 *
 */
public class AndroidVideoUI extends AndroidMediaUI<VideoField> implements OnCompletionListener {

	static private final String TAG = "AndroidVideoUI";

	// Camera & image data:
	private CameraController cameraController;
	private SurfaceView captureSurface;

	private volatile Boolean recording = false;
	private int playbackPosition = 0;

	public AndroidVideoUI(VideoField field, Controller controller,
			CollectorView collectorUI) {
		super(field, controller, collectorUI);
	}

	@SuppressWarnings("deprecation")
	@Override
	void populateCaptureLayout(ViewGroup captureLayout) {
		if (cameraController == null) {
			// Set up cameraController:
			//	Camera controller & camera selection:
			cameraController =
					new CameraController(field.isUseFrontFacingCamera());
			if (!cameraController.foundCamera()) { // no camera found, try the other one:
				cameraController.findCamera(!field.isUseFrontFacingCamera());
				if (!cameraController.foundCamera()) { // still no camera, this device does not seem to have one:
					mediaDone(null, false);
					return;
				}
			}
		}
		//TODO figure out how views are cached so this does not have to be done every time:
		captureLayout.removeAllViews();
		// Create the surface for previewing the camera:
		captureSurface = new SurfaceView(captureLayout.getContext());
		captureLayout.addView(captureSurface);

		// Set-up surface holder:
		SurfaceHolder holder = captureSurface.getHolder();
		holder.addCallback(cameraController);
		holder.setKeepScreenOn(true);
		// !!! Deprecated but cameraController preview crashes without it (at least on the XCover/Gingerbread):
		holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);

		cameraController.startPreview();
	}

	@Override
	boolean onCapture() {
			synchronized(recording) {
				if (!recording) {
					// start recording
					lastCaptureFile = field.getNewTempFile(controller.getFileStorageProvider(),controller.getCurrentRecord());
					cameraController.startVideoCapture(lastCaptureFile);
					recording = true;
				} else {
					// stop recording
					cameraController.stopVideoCapture();
					// a capture has been made so show it for review:
					attachMediaFile(false);
					recording = false;
				}
			}
		// always allow other click events after this completes (so recording can be stopped by pressing again):
		return true;
	}


	@Override
	void onDiscard() {
		// nothing to do
	}

	@Override
	void populateReviewLayout(ViewGroup reviewLayout, File mediaFile) {
		reviewLayout.removeAllViews();
		final VideoView playbackView = new VideoView(reviewLayout.getContext());

		playbackView.setOnTouchListener(new OnTouchListener() {
			@Override
			public boolean onTouch(View v, MotionEvent ev) {
				if (ev.getAction() == MotionEvent.ACTION_UP) { 
					// only perform action when finger is lifted off screen
					if (playbackView.isPlaying()) {
						Log.d(TAG,"Pausing video...");
						playbackPosition = playbackView.getCurrentPosition();
						playbackView.pause();
					}

					else {
						Log.d(TAG,"Playing video...");
						playbackView.seekTo(playbackPosition);
						playbackView.start();
					}
				}
				return true;
			}
		});
		LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.MATCH_PARENT);
		params.gravity = Gravity.CENTER_HORIZONTAL;
		playbackView.setLayoutParams(params);
		playbackView.setOnCompletionListener(this);
		reviewLayout.addView(playbackView);
		playbackView.setVideoURI(Uri.fromFile(mediaFile));
		playbackView.start();
	}
	

	@Override
    public void onCompletion(MediaPlayer mp) {
	    // playback has finished so go back to start
		playbackPosition = 0;
    }

	/**
	 * If not currently recording, will return a "start recording" button. If currently recording, will return a
	 * "stop recording" button.
	 */
	@Override
	ImageItem generateCaptureButton(Context context) { //TODO: allow for specific "video" button
		ImageItem captureButton = null;
		if (!recording) {
			// recording hasn't started yet, so present "record" button
			File captureImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(),field.getCaptureButtonImageRelativePath());
			if(FileHelpers.isReadableFile(captureImgFile))
				// use a custom video capture image if available
				captureButton = new FileImageItem(captureImgFile);
			else
				// otherwise just use the default resource
				captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_video_capture_svg);
		}
		else {
			// recording started, so present "stop" button instead
			File stopImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(),field.getStopRecImageRelativePath());
			if(FileHelpers.isReadableFile(stopImgFile))
				captureButton = new FileImageItem(stopImgFile);
			else
				captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_stop_audio_svg); //TODO video
		}
		captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return captureButton;
	}

	@Override
	List<Item> getMediaItems() {
		// TODO: assumes all attachments (regardless of type) are relevant to this field!
		// TODO: cache?
		List<File> files = controller.getMediaAttachments();
		List<Item> items = new ArrayList<Item>();
		for (File f : files) {
			items.add(new VideoItem(f)); //TODO will fail if file is not video -- should change when refactored
		}
		return items;
	}

	@Override
	void finalise() {
		if(cameraController != null)
			cameraController.close();	    
	}

}