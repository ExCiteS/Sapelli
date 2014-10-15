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
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
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
import uk.ac.ucl.excites.sapelli.storage.model.Record;
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
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.LinearLayout;

/**
 * A subclass of AndroidMediaUI which allows for the capture and 
 * review of images from the device's camera.
 * 
 * TODO added photo/no photo buttons before entering actual camera mode OR going to naive camera app (which is no longer called from the controller!)
 * 
 * TODO Fix white "blocks" briefly appearing where the button(s) should be when view is loaded for 2nd time (on Android v2.x only?)
 * 
 * @author mstevens, Michalis Vitos, benelliott
 *
 */
public class AndroidPhotoUI extends AndroidMediaUI<PhotoField> implements PictureCallback {

	@SuppressWarnings("unused")
    static private final String TAG = "AndroidPhotoUI";
	static private final int PREVIEW_SIZE = 1024;

	// Camera & image data:
	private CameraController cameraController;
	private SurfaceView captureSurface;
	private HandleImage handleImage;

	public AndroidPhotoUI(PhotoField field, Controller controller,
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
	        //	Set flash mode:
	        cameraController.setFlashMode(field.getFlashMode());
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
		cameraController.takePicture(this);
		return false; // only allow new clicks once photo has been received
	}
	

	@Override
    void onDiscard() {
	    // nothing to do
    }

	@Override
	void populateReviewLayout(ViewGroup reviewLayout, File mediaFile) {
		reviewLayout.removeAllViews();
		// add an ImageView to the review UI:
		ImageView reviewView = new ImageView(reviewLayout.getContext());
		reviewView.setScaleType(ScaleType.FIT_CENTER);
		// make sure the image takes up all the available space:
		LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT);
		reviewView.setLayoutParams(params);
		// set the ImageView to the provided photo file:
		reviewView.setImageURI(Uri.fromFile(mediaFile));
		reviewLayout.addView(reviewView);
	}

	@Override
	ImageItem generateCaptureButton(Context context) {
		ImageItem captureButton = null;
		File captureImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(),field.getCaptureButtonImageRelativePath());
		if(FileHelpers.isReadableFile(captureImgFile))
			// return a custom photo capture button if it exists
			captureButton = new FileImageItem(captureImgFile);
		else
			// otherwise just use the default resource
			captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_photo_svg);
		captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return captureButton;
	}

	List<Item> getMediaItems(FileStorageProvider fileStorageProvider, Record record) {
		List<Item> items = new ArrayList<Item>();
		for (File f : field.getAttachments(fileStorageProvider, record)) {
			items.add(new FileImageItem(f));
		}
		return items;
	}
	
	@Override
    void finalise() {
		if(cameraController != null)
			cameraController.close();	    
	}
	
	@Override
	public void onPictureTaken(byte[] data, Camera camera)
	{
		handleImage = new HandleImage(data, getContext());
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
		private Context context;

		public HandleImage(byte[] data, Context context)
		{
			this.data = data;
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
				
				// write to temporary file right away, but don't attach it unless approved
				// may eventually run out of storage but makes media code easier - should be fixed 
				// when temp file stuff is refactored --- TODO
				lastCaptureFile = field.getNewMediaFile(controller.getFileStorageProvider(),controller.getCurrentRecord());
				FileOutputStream fos = new FileOutputStream(lastCaptureFile);
				fos.write(data);
				fos.close();
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
			cameraController.stopPreview();
			// Close the dialog
			dialog.cancel();
			attachMediaFile(true);
		}
	}
}