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

import android.app.ProgressDialog;
import android.content.Context;
import android.hardware.Camera;
import android.hardware.Camera.PictureCallback;
import android.os.AsyncTask;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.shared.util.android.BitmapUtils;

/**
 * A subclass of AndroidMediaUI which allows for the capture and review of images from the device's camera.
 * 
 * NOTE: Samsung decided not to bother to enable portrait photo/video capture in the xCover 1 kernel, so captures may display incorrectly on that model.
 * -- See http://stackoverflow.com/questions/19176038
 * 
 * TODO Fix white "blocks" briefly appearing where the button(s) should be when view is loaded for 2nd time (on Android v2.x only?)
 * 
 * @author mstevens, Michalis Vitos, benelliott
 */
public class AndroidPhotoUI extends AndroidCameraUI<PhotoField> implements PictureCallback
{
	
	static protected final String TAG = "AndroidPhotoUI";

	public AndroidPhotoUI(PhotoField field, CollectorController controller, CollectorView collectorUI)
	{
		super(	field,
				controller,
				collectorUI,
				false);	// do not allow clicks as the capture process (see onCapture()) is asynchronous and returns immediately
	}
		
	@Override
	protected View createCaptureContent(Context context)
	{
		return getCaptureContent(context, false, field.isUseFrontFacingCamera(), field.getFlashMode());
	}
	
	@Override
	protected ImageItem generateCaptureButton(Context context)
	{
		return collectorUI.getImageItemFromProjectFileOrResource(field.getCaptureButtonImageRelativePath(), R.drawable.button_photo_capture);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.AndroidCameraUI#doCapture()
	 */
	@Override
	protected void doCapture()
	{
		try
		{
			cameraController.takePicture(this); // handling of image is asynchronous !!!
		}
		catch(Exception e)
		{
			handleCaptureError(e);
		}
	}
	
	@Override
	protected View getReviewContent(Context context, File mediaFile)
	{
		// add an ImageView to the review UI:
		ImageView reviewView = new ImageView(context);
		reviewView.setScaleType(ScaleType.FIT_CENTER);
		reviewView.setBackgroundColor(fieldBackgroundColor);
		// set the ImageView to the provided photo file:
		reviewView.setImageBitmap(BitmapUtils.loadBitmap(context, mediaFile));
		return reviewView;
	}

	@Override
	protected Item<?> getGalleryItem(int index, File photoFile)
	{
		// TODO use EXIF data to determine proper rotation? Cf. http://stackoverflow.com/q/12944123/1084488
		/*// Old example code to rotate bitmap (would have to be integrated in (File)ImageItem):
		Matrix bitmapMatrix = new Matrix();
		bitmapMatrix.postRotate(90);
		bitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), bitmapMatrix, false);*/
		
		return new ImageItem(index, photoFile); // will use BitmapUtils for memory-safe scaling
	}
	
	@Override
	protected int getCameraErrorStringId(boolean fatal)
	{
		return fatal ? R.string.photoCameraErrorFatal : R.string.photoCameraErrorSkip;
	}
	
	@Override
	public void onPictureTaken(byte[] data, Camera camera)
	{
		new HandleImage().execute(data);
	}

	/**
	 * AsyncTask to handle the Captured Images
	 * 
	 * @author Michalis Vitos, benelliott, mstevens
	 * 
	 */
	public class HandleImage extends AsyncTask<byte[], Void, File>
	{
		
		private ProgressDialog dialog;
		private Exception error;

		@Override
		protected void onPreExecute()
		{
			dialog = new ProgressDialog(collectorUI.getContext());
			dialog.setCancelable(false);
			dialog.show();
		}

		@Override
		protected File doInBackground(byte[]... data)
		{
			File photoFile = getNewCaptureFile();
			try
			{
				FileOutputStream fos = new FileOutputStream(photoFile);
				fos.write(data[0]);
				fos.close();
			}
			catch(Exception e)
			{
				error = e;
				return null; // !!!
			}
			return photoFile;
		}

		@Override
		protected void onPostExecute(File photoFile)
		{
			cameraController.stopPreview();
			
			// Close the dialog:
			dialog.cancel();
			
			// Continue...
			if(photoFile != null)
				handleCaptureSuccess(); // attach photo (not need to pass reference, super class has remembered the captureFile)
			else
				handleCaptureError(error); // report failure
			
			// Allow clicks now we have finished
			controller.unblockUI(); // !!!
		}
		
	}
	
}
