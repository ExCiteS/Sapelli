package uk.ac.ucl.excites.sapelli.collector.media;

import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField.FlashMode;
import android.graphics.ImageFormat;
import android.hardware.Camera;
import android.hardware.Camera.AutoFocusCallback;
import android.hardware.Camera.CameraInfo;
import android.hardware.Camera.PictureCallback;
import android.util.Log;
import android.view.SurfaceHolder;

/**
 * Camera operator class
 * 
 * Based on:
 * 	- http://stackoverflow.com/a/6421467/1084488
 * 	- http://stackoverflow.com/a/8003222/1084488
 * 	- http://www.vogella.com/code/de.vogella.camera.api/codestartpage.html
 *  - http://android-er.blogspot.co.uk/2012/08/determine-best-camera-preview-size.html
 * 
 * To do's:
 * 	- TODO Fix issue: auto flash mode never seems to flash (on XCover)
 *  - TODO Capture using hardware shutter button
 * 
 * @author mstevens
 */
public class CameraController implements SurfaceHolder.Callback
{

	static private final String TAG = "CameraController";

	static private final int NO_CAMERA_FOUND = -1;

	private Camera camera;
	private int cameraID = NO_CAMERA_FOUND;
	private PhotoField.FlashMode flashMode = PhotoField.DEFAULT_FLASH_MODE;
	private boolean inPreview = false;
	private boolean cameraConfigured = false;

	public CameraController()
	{
		this(false);
	}

	public CameraController(boolean frontFacing)
	{
		this.cameraID = findCamera(frontFacing);
	}

	public int findCamera(boolean frontFacing)
	{
		for(int c = 0; c < Camera.getNumberOfCameras(); c++)
		{
			CameraInfo info = new CameraInfo();
			Camera.getCameraInfo(c, info);
			if(info.facing == (frontFacing ? CameraInfo.CAMERA_FACING_FRONT : CameraInfo.CAMERA_FACING_BACK))
				return c;
		}
		return NO_CAMERA_FOUND;
	}

	public void setFlashMode(FlashMode flashMode)
	{
		this.flashMode = flashMode;
	}

	public boolean foundCamera()
	{
		return(cameraID != NO_CAMERA_FOUND);
	}

	public void startPreview()
	{
		if(!inPreview && cameraConfigured && camera != null)
		{
			camera.startPreview();
			inPreview = true;
		}
	}

	public void stopPreview()
	{
		if(camera != null)
			camera.stopPreview();
		inPreview = false;
	}

	public void takePicture(final PictureCallback callback)
	{
		if(camera != null)
		{
			// Use auto focus if the camera supports it
			String focusMode = camera.getParameters().getFocusMode();
			if(focusMode.equals(Camera.Parameters.FOCUS_MODE_AUTO) || focusMode.equals(Camera.Parameters.FOCUS_MODE_MACRO))
			{
				camera.autoFocus(new AutoFocusCallback()
				{
					@Override
					public void onAutoFocus(boolean success, Camera camera)
					{
						camera.takePicture(null, null, callback);
					}
				});
			}
			else
				camera.takePicture(null, null, callback);
		}
	}

	public void close()
	{
		if(camera != null)
		{
			stopPreview();
			camera.release();
			camera = null;
			cameraConfigured = false;
		}
	}

	@Override
	public void surfaceCreated(SurfaceHolder holder)
	{
		if(foundCamera())
		{
			try
			{
				camera = Camera.open(cameraID);
			}
			catch(Exception e)
			{
				Log.e(TAG, "Could not open camera.", e);
			}
		}
	}

	@Override
	public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
	{
		if(camera != null && holder.getSurface() != null)
		{
			try
			{
				camera.setPreviewDisplay(holder);
			}
			catch(Throwable t)
			{
				Log.e(TAG, "Exception in setPreviewDisplay()", t);
				return;
			}
			if(!cameraConfigured)
			{
				camera.setDisplayOrientation(90); // TODO optionally make this change with device orientation?
				Camera.Parameters parameters = camera.getParameters();

				// Preview size:
				Camera.Size previewSize = getBestPreviewSize(width, height, parameters);
				if(previewSize != null)
					parameters.setPreviewSize(previewSize.width, previewSize.height);

				// Set scene mode:
				List<String> sceneModes = parameters.getSupportedSceneModes();
				if(sceneModes != null && sceneModes.contains(Camera.Parameters.SCENE_MODE_AUTO))
					parameters.setSceneMode(Camera.Parameters.SCENE_MODE_AUTO);

				// Set white balance:
				List<String> whiteBalanceModes = parameters.getSupportedWhiteBalance();
				if(whiteBalanceModes != null && whiteBalanceModes.contains(Camera.Parameters.WHITE_BALANCE_AUTO))
					parameters.setWhiteBalance(Camera.Parameters.WHITE_BALANCE_AUTO);

				// Set focus mode:
				if(parameters.getSupportedFocusModes().contains(Camera.Parameters.FOCUS_MODE_AUTO))
					parameters.setFocusMode(Camera.Parameters.FOCUS_MODE_AUTO);

				// Flash mode:
				try
				{
					parameters.setFlashMode(getAppropriateFlashMode(parameters));
				}
				catch(NullPointerException e)
				{
					Log.e(TAG, "Exception in setFlashMode()", e);
				}

				//Resulting file:
				//	Format:
				parameters.setPictureFormat(ImageFormat.JPEG);
				parameters.set("jpeg-quality", 100);
				//	Size:
				Camera.Size pictureSize = getLargestPictureSize(parameters);
				if(pictureSize != null)
					parameters.setPictureSize(pictureSize.width, pictureSize.height);

				camera.setParameters(parameters);
				cameraConfigured = true;
			}
		}
		startPreview();
	}

	@Override
	public void surfaceDestroyed(SurfaceHolder holder)
	{
		close();
	}

	private String getAppropriateFlashMode(Camera.Parameters parameters)
	{
		List<String> availableModes = parameters.getSupportedFlashModes();
		if(availableModes != null)
		{
			switch(flashMode)
			{
				case ON :	if(availableModes.contains(Camera.Parameters.FLASH_MODE_ON))
								return Camera.Parameters.FLASH_MODE_ON;
							break;
				case AUTO :	if(availableModes.contains(Camera.Parameters.FLASH_MODE_AUTO))
								return Camera.Parameters.FLASH_MODE_AUTO;
							break;
				case OFF :	if(availableModes.contains(Camera.Parameters.FLASH_MODE_OFF))
								return Camera.Parameters.FLASH_MODE_OFF;
							break;
			}
		}
		return parameters.getFlashMode(); //leave as is
	}
	
	private Camera.Size getBestPreviewSize(int width, int height, Camera.Parameters parameters)
	{
		Camera.Size result = null;
		for(Camera.Size size : parameters.getSupportedPreviewSizes())
		{
			if(size.width <= width && size.height <= height)
			{
				if(result == null)
					result = size;
				else
				{
					int resultArea = result.width * result.height;
					int newArea = size.width * size.height;
					if(newArea > resultArea)
						result = size;
				}
			}
		}
		return result;
	}

	private Camera.Size getLargestPictureSize(Camera.Parameters parameters)
	{
		Camera.Size result = null;
		for(Camera.Size size : parameters.getSupportedPictureSizes())
		{
			if(result == null)
				result = size;
			else
			{
				if(size.width * size.height > result.width * result.height)
					result = size;
			}
		}
		return result;
	}

}
