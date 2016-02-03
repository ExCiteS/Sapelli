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

package uk.ac.ucl.excites.sapelli.collector.media;

import java.io.File;
import java.io.FileOutputStream;
import java.util.List;

import android.annotation.SuppressLint;
import android.graphics.ImageFormat;
import android.hardware.Camera;
import android.hardware.Camera.AutoFocusCallback;
import android.hardware.Camera.CameraInfo;
import android.hardware.Camera.PictureCallback;
import android.media.CamcorderProfile;
import android.media.MediaRecorder;
import android.os.Build;
import android.util.Log;
import android.view.SurfaceHolder;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField.FlashMode;

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

	// STATICS ------------------------------------------------------
	static private final String TAG = "CameraController";

	static private final int NO_CAMERA_FOUND = -1;
	static private final int ROTATION = 90;
	static private final int VIDEO_CAPTURE_QUALITY = CamcorderProfile.QUALITY_HIGH; // TODO decide which quality
	
	// DYNAMIC ------------------------------------------------------
	private int cameraID = NO_CAMERA_FOUND;
	private Camera camera;
	private PhotoField.FlashMode flashMode = PhotoField.DEFAULT_FLASH_MODE;
	private boolean inPreview = false;
	
	private MediaRecorder videoRecorder;
	private FileOutputStream videoFos;
	private boolean recordingHint; // if set to true, reduces time to start video recording
	
	private volatile boolean recording;

	/**
	 * Generic camera use, not optimised for video
	 */
	public CameraController()
	{
		this(false);
	}
	
	/**
	 * Specific camera use
	 * 
	 * @param recordingHint pass true when video recording will happen
	 */
	public CameraController(boolean recordingHint)
	{
		this.recordingHint = recordingHint;
	}

	/**
	 * @param frontFacing whether or not to use the front-facing camera
	 * @return camera index
	 */
	public int setCamera(boolean frontFacing)
	{
		for(int c = 0; c < Camera.getNumberOfCameras(); c++)
		{
			CameraInfo info = new CameraInfo();
			Camera.getCameraInfo(c, info);
			if(info.facing == (frontFacing ? CameraInfo.CAMERA_FACING_FRONT : CameraInfo.CAMERA_FACING_BACK))
				return cameraID = c;
		}
		return NO_CAMERA_FOUND;
	}

	public boolean foundCamera()
	{
		return cameraID != NO_CAMERA_FOUND;
	}

	public void setFlashMode(FlashMode flashMode)
	{
		this.flashMode = flashMode;
	}
	
	private void open()
	{
		try
		{
			if(!foundCamera())
				throw new IllegalStateException("No camera set/found");
			camera = Camera.open(cameraID);
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not open camera.", e);
		}
	}
	
	public void close()
	{
		stopPreview();
		stopVideoCapture();
		if(camera != null)
		{
			camera.release();
			camera = null;
		}
		if(videoRecorder != null)
		{
			videoRecorder.release();
			videoRecorder = null;
		}
	}
	
	private void configure(Camera.Size previewSize) throws Exception
	{
		if(camera == null) // just in case
			return;
		
		// Configure camera (parameters):
		Camera.Parameters parameters = camera.getParameters();
		
		// Preview size:
		if(previewSize != null)
			parameters.setPreviewSize(previewSize.width, previewSize.height);
		
		// 	Preview orientation:
		camera.setDisplayOrientation(ROTATION); // TODO optionally make this change with device orientation?
		
		//	IMAGE orientation (as opposed to preview):
		parameters.setRotation(ROTATION); // should match preview
		
		//	Set recording hint (if supported):
		setVideoRecordingHint(parameters);

		//	Set scene mode:
		List<String> sceneModes = parameters.getSupportedSceneModes();
		if(sceneModes != null && sceneModes.contains(Camera.Parameters.SCENE_MODE_AUTO))
			parameters.setSceneMode(Camera.Parameters.SCENE_MODE_AUTO);

		//	Set white balance:
		List<String> whiteBalanceModes = parameters.getSupportedWhiteBalance();
		if(whiteBalanceModes != null && whiteBalanceModes.contains(Camera.Parameters.WHITE_BALANCE_AUTO))
			parameters.setWhiteBalance(Camera.Parameters.WHITE_BALANCE_AUTO);

		//	Set focus mode:
		if(parameters.getSupportedFocusModes().contains(Camera.Parameters.FOCUS_MODE_AUTO))
			parameters.setFocusMode(Camera.Parameters.FOCUS_MODE_AUTO);

		//	Flash mode:
		String flashMode = getAppropriateFlashMode(parameters); // may be null if flash mode setting is unavailable
		if(flashMode != null && !flashMode.isEmpty())
			parameters.setFlashMode(flashMode);
		
		//Resulting file:
		//	Format:
		parameters.setPictureFormat(ImageFormat.JPEG);
		parameters.set("jpeg-quality", 100);
		//	Size:
		Camera.Size pictureSize = getLargestPictureSize(parameters);
		if(pictureSize != null)
			parameters.setPictureSize(pictureSize.width, pictureSize.height);

		camera.setParameters(parameters);
	}
	
	public void startPreview()
	{
		if(!inPreview && camera != null)
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

	public void takePicture(final PictureCallback callback) throws Exception
	{
		if(camera == null)
			throw new IllegalStateException("Camera unavailable!");
		
		// TODO lock camera here?
			
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
	
	public void startVideoCapture(File outputFile) throws Exception
	{
		if(camera == null)
			throw new IllegalStateException("Camera unavailable!");
		
		// set up a FileOutputStream for the output file:
		videoFos = new FileOutputStream(outputFile);

		// unlock the camera for use by MediaRecorder:
		camera.unlock(); // TODO explain?

		// Get/reset videoRecorder:
		if(videoRecorder == null)
			videoRecorder = new MediaRecorder();
		else
			videoRecorder.reset();
		
		// configure MediaRecorder (MUST call in this order):
		videoRecorder.setCamera(camera);
		videoRecorder.setAudioSource(MediaRecorder.AudioSource.CAMCORDER);
		videoRecorder.setVideoSource(MediaRecorder.VideoSource.CAMERA);
		videoRecorder.setProfile(CamcorderProfile.get(cameraID, VIDEO_CAPTURE_QUALITY));
		// TODO enforce mp4?
		videoRecorder.setOrientationHint(ROTATION);
		videoRecorder.setOutputFile(videoFos.getFD());
		// Note: no need to call videoRecorder.setPreviewDisplay(previewSurface); because the surface has already been set on the camera
		
		// prepare MediaRecorder:
		videoRecorder.prepare();
		// start MediaRecorder (and start recording video):
		videoRecorder.start();
		
		// If no execptions thrown above...
		recording = true;
	}
	
	public void stopVideoCapture()
	{
		recording = false;
		if(videoRecorder != null)
		{
			try
			{
				videoRecorder.stop();
				videoFos.close();
			}
			catch(Exception ignore) {}
			videoFos = null;
		}
		if(camera != null)
			camera.lock(); // TODO explain?
	}
	
	/**
	 * @return the recording
	 */
	public boolean isRecording()
	{
		return videoRecorder != null && recording;
	}

	@Override
	public void surfaceCreated(SurfaceHolder holder)
	{
		open(); // open camera
	}

	@Override
	public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
	{
		if(camera != null && holder.getSurface() != null)
		{
			try
			{
				// Register preview surface:
				camera.setPreviewDisplay(holder);
				
				// (Re)configure camera:
				configure(getBestPreviewSize(width, height));
			}
			catch(Throwable t)
			{
				Log.e(TAG, "Exception in setPreviewDisplay()", t);
				return;
			}
		}
		startPreview();
	}

	@Override
	public void surfaceDestroyed(SurfaceHolder holder)
	{
		close(); // stop preview & close camera
	}

	/**
	 * Set recording hint (not supported below API level 14)
	 * 
	 * @param parameters
	 */
	@SuppressLint("NewApi")
	private void setVideoRecordingHint(Camera.Parameters parameters)
	{
		if(Build.VERSION.SDK_INT >= 14)
			parameters.setRecordingHint(recordingHint);
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
	
	private Camera.Size getBestPreviewSize(int width, int height)
	{
		if(camera == null) // check if there is a camera
			return null;
		Camera.Size result = null;
		for(Camera.Size size : camera.getParameters().getSupportedPreviewSizes())
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
