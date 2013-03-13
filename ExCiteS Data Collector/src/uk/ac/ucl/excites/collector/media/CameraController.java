package uk.ac.ucl.excites.collector.media;

import android.hardware.Camera;
import android.hardware.Camera.CameraInfo;
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
 * 	- + some other posts on stackoverflow
 * 
 * @author mstevens
 */
public class CameraController implements SurfaceHolder.Callback
{

	static private final String TAG = "CameraController";

	static private final int NO_CAMERA_FOUND = -1;

	private Camera camera;
	private int cameraID = NO_CAMERA_FOUND;
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

	public void takePicture()
	{
		if(camera != null)
		{
			
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
				
				//TODO flash support
//				mSupportedFlashModes = mCamera.getParameters().getSupportedFlashModes();
//		        // Set the camera to Auto Flash mode.
//		        if (mSupportedFlashModes.contains(Camera.Parameters.FLASH_MODE_AUTO))
//		        {
//		            Camera.Parameters parameters = mCamera.getParameters();
//		            parameters.setFlashMode(Camera.Parameters.FLASH_MODE_AUTO);             
//		            mCamera.setParameters(parameters);
//		        }
				
				Camera.Size size = getBestPreviewSize(width, height, parameters);
				if(size != null)
				{
					parameters.setPreviewSize(size.width, size.height);
					camera.setParameters(parameters);
					cameraConfigured = true;
				}
			}
		}
		startPreview();
	}

	@Override
	public void surfaceDestroyed(SurfaceHolder holder)
	{
		close();
	}

	private Camera.Size getBestPreviewSize(int width, int height, Camera.Parameters parameters)
	{
		Camera.Size result = null;
		for(Camera.Size size : parameters.getSupportedPreviewSizes())
		{
			if(size.width <= width && size.height <= height)
			{
				if(result == null)
				{
					result = size;
				}
				else
				{
					int resultArea = result.width * result.height;
					int newArea = size.width * size.height;

					if(newArea > resultArea)
					{
						result = size;
					}
				}
			}
		}
		return result;
	}

}
