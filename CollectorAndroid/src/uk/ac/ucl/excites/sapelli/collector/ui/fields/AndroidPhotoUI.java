package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.media.CameraController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.BitmapUtils;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.ProgressDialog;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.hardware.Camera;
import android.hardware.Camera.PictureCallback;
import android.net.Uri;
import android.os.AsyncTask;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

public class AndroidPhotoUI extends AndroidMediaUI<PhotoField> implements PictureCallback {
	
	static private final int PREVIEW_SIZE = 1024;
	
	private ImageView reviewView;
	// Camera & image data:
	private CameraController cameraController;
	private HandleImage handleImage;

	public AndroidPhotoUI(PhotoField field, Controller controller,
            CollectorView collectorUI) {
	    super(field, controller, collectorUI);
    }
	
	@SuppressWarnings("deprecation")
    @Override
    public void populateCaptureLayout(View captureLayout) {
		// Set up cameraController:
		//	Camera controller & camera selection:
		cameraController = new CameraController(field.isUseFrontFacingCamera());
		if(!cameraController.foundCamera())
		{ // no camera found, try the other one:
			cameraController.findCamera(!field.isUseFrontFacingCamera());
			if(!cameraController.foundCamera())
			{ // still no camera, this device does not seem to have one:
				mediaDone(null, false);
				return;
			}
		}
		//	Set flash mode:
		cameraController.setFlashMode(field.getFlashMode());

		// Create the surface for previewing the camera:
		final SurfaceView surfaceView = (SurfaceView) captureLayout.findViewById(R.id.capture_layout_surface);

		// Set-up surface holder:
		SurfaceHolder holder = surfaceView.getHolder();
		holder.addCallback(cameraController);
		holder.setKeepScreenOn(true);
		// !!! Deprecated but cameraController preview crashes without it (at least on the XCover/Gingerbread):
		holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
    }
	
	@Override
    public void populateReviewLayout(View reviewLayout) {
		reviewView = (ImageView) reviewLayout.findViewById(R.id.review_layout_imageview);
		reviewView.setScaleType(ScaleType.FIT_CENTER);
    }

	@Override
    public void onCapture() {
		cameraController.takePicture(this);	    
    }

	@Override
    public void onMediaSaved() {
	    cameraController.close(); 
    }

	@Override
    public void onCaptureStarted() {
		cameraController.startPreview();    
    }

	@Override
    public void populateDeleteLayout(View deleteLayout, File mediaFile) {
		ImageView deletePhotoView = (ImageView) deleteLayout.findViewById(R.id.review_layout_imageview);
		deletePhotoView.setScaleType(ScaleType.FIT_CENTER);
		// set the ImageView to the provided photo file:
		deletePhotoView.setImageURI(Uri.fromFile(mediaFile));	    
    }

	@Override
    public void finalise() {
		if(cameraController != null)
			cameraController.close();	    
    }

	@Override
    public ImageItem getCaptureButton(Context context) {
		ImageItem captureButton = null;
		if (context == null) Log.d("PhotoUI","null context");
		else Log.d("PhotoUI","context not null");
		File captureImgFile = controller.getProject().getImageFile(field.getCaptureButtonImageRelativePath());
		if(FileHelpers.isReadableFile(captureImgFile))
			captureButton = new FileImageItem(captureImgFile);
		else
			captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_photo_svg);
		captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return captureButton;
    }

	@Override
    public List<Item> getMediaItems() {
	    List<File> files = controller.getMediaAttachments();
	    List<Item> items = new ArrayList<Item>();
	    for (File f : files) {
	    	items.add(new FileImageItem(f));
	    }
	    return items;
    }

	@Override
	public void onPictureTaken(byte[] data, Camera camera)
	{
		capturedMediaData = data;
		handleImage = new HandleImage(data, reviewView, getContext());
		handleImage.execute();
	}

	/**
	 * AsyncTask to handle the Captured Images
	 * 
	 * @author Michalis Vitos
	 * 
	 */
	public class HandleImage extends AsyncTask<Void, Void, Bitmap>
	{
		private ProgressDialog dialog;
		private byte[] data;
		private ImageView reviewView;
		private Context context;

		public HandleImage(byte[] data, ImageView reviewView, Context context)
		{
			this.data = data;
			this.reviewView = reviewView;
			this.context = context;
		}

		@Override
		protected void onPreExecute()
		{
			dialog = new ProgressDialog(context);
			dialog.setCancelable(false);
			dialog.show();
		}

		@Override
		protected Bitmap doInBackground(Void... params)
		{
			Bitmap picture = null;

			try
			{
				// TODO use EXIF data to determine proper rotation? Cf. http://stackoverflow.com/q/12944123/1084488

				// Decode image size, do not create the actual bitmap (picture is null)
				BitmapFactory.Options options = new BitmapFactory.Options();
				options.inJustDecodeBounds = true;
				picture = BitmapFactory.decodeByteArray(data, 0, data.length, options);

				// Find the preview size
				int previewWidth = (ScreenMetrics.GetScreenWidth(context) > 0) ? ScreenMetrics.GetScreenWidth(context) : PREVIEW_SIZE;
				int previewHeight = (ScreenMetrics.GetScreenHeight(context) > 0) ? ScreenMetrics.GetScreenHeight(context) : PREVIEW_SIZE;

				// Decode with inSampleSize and get the correct, scaled image
				options.inJustDecodeBounds = false;
				options.inSampleSize = BitmapUtils.calculateInSampleSize(options, previewWidth, previewHeight);
				picture = BitmapFactory.decodeByteArray(data, 0, data.length, options);
			}
			catch(Exception e)
			{
				Log.e("Handle image", "Image capture failed");
				Debug.e(e);
			}
			return picture;
		}

		@Override
		protected void onPostExecute(Bitmap picture)
		{
			// Display rotated picture
			reviewView.setImageBitmap(picture);
			// Switch to review mode:
			cameraController.stopPreview();
			// Close the dialog
			dialog.cancel();
			dataReceived();
		}
	}
}
