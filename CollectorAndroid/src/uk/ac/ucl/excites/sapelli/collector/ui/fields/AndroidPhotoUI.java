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
 * A subclass of AndroidMediaUI which allows for the capture and review of images from the device's camera.
 * 
 * NOTE: Samsung decided not to bother to enable portrait photo/video capture in the xCover 1 kernel, so captures may display incorrectly on that model.
 * -- See http://stackoverflow.com/questions/19176038/
 * 
 * TODO added photo/no photo buttons before entering actual camera mode OR going to naive camera app (which is no longer called from the controller!)
 * 
 * TODO Fix white "blocks" briefly appearing where the button(s) should be when view is loaded for 2nd time (on Android v2.x only?)
 * 
 * @author mstevens, Michalis Vitos, benelliott
 */
public class AndroidPhotoUI extends AndroidMediaUI<PhotoField> implements PictureCallback
{
	
	static protected final String TAG = "AndroidPhotoUI";

	// Camera & image data:
	private CameraController cameraController;
	private SurfaceView captureSurface;

	public AndroidPhotoUI(PhotoField field, CollectorController controller, CollectorView collectorUI)
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
			{ 	// No camera found, try the other one:
				cameraController.findCamera(!field.isUseFrontFacingCamera());
				if(!cameraController.foundCamera())
				{	// Still no camera, this device does not seem to have one:
					attachMedia(null);
					if(isValid(controller.getCurrentRecord()))
						controller.goForward(false);
					else
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_NO_STORAGE);
					return null;
				}
			}
			// Set flash mode:
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
	protected View getReviewContent(Context context, File mediaFile)
	{
		// add an ImageView to the review UI:
		ImageView reviewView = new ImageView(context);
		reviewView.setScaleType(ScaleType.FIT_CENTER);
		// set the ImageView to the provided photo file:
		reviewView.setImageURI(Uri.fromFile(mediaFile));
		return reviewView;
	}

	@Override
	protected boolean onCapture()
	{
		cameraController.takePicture(this);
		// do not allow clicks yet as the above call is asynchronous and returns immediately
		return false;
	}
	
	@Override
	protected void onDiscard()
	{
		// nothing to do
	}
	@Override
	protected ImageItem<?> generateCaptureButton(Context context)
	{
		ImageItem<?> captureButton = null;
		File captureImgFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), field.getCaptureButtonImageRelativePath());
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
	protected Item<?> getItemForAttachment(int index, File photoFile)
	{
		// TODO use EXIF data to determine proper rotation? Cf. http://stackoverflow.com/q/12944123/1084488
		/*// Old example code to rotate bitmap (would have to be integrated in (File)ImageItem):
		Matrix bitmapMatrix = new Matrix();
		bitmapMatrix.postRotate(90);
		bitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), bitmapMatrix, false);*/
		
		return new FileImageItem(index, photoFile); // will use BitmapUtils for memory-safe scaling
	}

	@Override
	protected void cancel()
	{
		super.cancel();
		if(cameraController != null)
			cameraController.close();
		cameraController = null;
	}
	
	@Override
	public void onPictureTaken(byte[] data, Camera camera)
	{
		new HandleImage(data).execute();
	}

	/**
	 * AsyncTask to handle the Captured Images
	 * 
	 * @author Michalis Vitos, benelliott
	 * 
	 */
	public class HandleImage extends AsyncTask<Void, Void, Void>
	{
		
		private ProgressDialog dialog;
		private byte[] data;

		public HandleImage(byte[] data)
		{
			this.data = data;
		}

		@Override
		protected void onPreExecute()
		{
			dialog = new ProgressDialog(collectorUI.getContext());
			dialog.setCancelable(false);
			dialog.show();
		}

		@Override
		protected Void doInBackground(Void... params)
		{
			try
			{
				captureFile = field.getNewAttachmentFile(controller.getFileStorageProvider() ,controller.getCurrentRecord());
				FileOutputStream fos = new FileOutputStream(captureFile);
				fos.write(data);
				fos.close();
				attachMedia(captureFile);
			}
			catch(Exception e)
			{
				Log.e("Handle image", "Image capture failed", e);
			}
			return null;
		}

		@Override
		protected void onPostExecute(Void result)
		{
			cameraController.stopPreview();
			
			// Close the dialog:
			dialog.cancel();
			
			if(field.isShowReview())
				controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
			else
				controller.goForward(true);
			
			// Allow clicks now we have finished
			controller.unblockUI(); // !!!
		}
		
	}
	
}
