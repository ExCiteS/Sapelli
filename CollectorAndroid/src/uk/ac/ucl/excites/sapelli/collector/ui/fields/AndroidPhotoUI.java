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

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.media.CameraController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.ProgressDialog;
import android.content.Context;
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

	// Camera & image data:
	private CameraController cameraController;
	private SurfaceView captureSurface;
	private HandleImage handleImage;

	public AndroidPhotoUI(PhotoField field, CollectorController controller, CollectorView collectorUI) {
		super(field, controller, collectorUI);
	}

	@SuppressWarnings("deprecation")
	@Override
	protected View getCaptureContent(Context context) {
		if (cameraController == null) {
	        // Set up cameraController:
	        //	Camera controller & camera selection:
	        cameraController =
	                new CameraController(field.isUseFrontFacingCamera());
	        if (!cameraController.foundCamera()) { // no camera found, try the other one:
		        cameraController.findCamera(!field.isUseFrontFacingCamera());
		        if (!cameraController.foundCamera()) { // still no camera, this device does not seem to have one:
		        	attachMedia(null);
					if (isValid(controller.getCurrentRecord()))
						controller.goForward(false);
					else
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_NO_STORAGE);
			        return null;
		        }
	        }
	        //	Set flash mode:
	        cameraController.setFlashMode(field.getFlashMode());
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
	protected View getReviewContent(Context context, File mediaFile) {
		// add an ImageView to the review UI:
		ImageView reviewView = new ImageView(context);
		reviewView.setScaleType(ScaleType.FIT_CENTER);
		// set the ImageView to the provided photo file:
		reviewView.setImageURI(Uri.fromFile(mediaFile));
		return reviewView;
	}

	@Override
	protected void onCapture() {
		cameraController.takePicture(this);
		// do not allow clicks yet as the above call is asynchronous and returns immediately
	}
	

	@Override
	protected void onDiscard() {
	    // nothing to do
    }

	@Override
	protected ImageItem generateCaptureButton(Context context) {
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
	
	@Override
	protected Item getItemFromFile(File file) {
		return new FileImageItem(file);
	}
	
	@Override
    protected void cancel() {
		super.cancel();
		if(cameraController != null)
			cameraController.close();
		cameraController = null;
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
	public class HandleImage extends AsyncTask<Void, Void, Void>
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
		protected Void doInBackground(Void... params)
		{

			try
			{
				captureFile = field.getNewAttachmentFile(controller.getFileStorageProvider(),controller.getCurrentRecord());
				FileOutputStream fos = new FileOutputStream(captureFile);
				fos.write(data);
				fos.close();
				attachMedia(captureFile);
			}
			catch(Exception e)
			{
				Log.e("Handle image", "Image capture failed");
				Debug.e(e);
			}
			
			return null;
		}

		@Override
		protected void onPostExecute(Void result)
		{
			cameraController.stopPreview();
			// Close the dialog
			dialog.cancel();
			if (field.isShowReview())
				controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
			else
				controller.goForward(true);
			// allow clicks now we have finished
			controller.unblockUI();
		}
	}
}