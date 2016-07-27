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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.AndroidCollectorController;
import uk.ac.ucl.excites.sapelli.collector.media.CameraController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField.FlashMode;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.sapelli.collector.ui.items.DrawableItem;
import android.content.Context;
import android.graphics.Color;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.widget.Toast;

/**
 * Abstract superclass for AndroidPhotoUI and AndroidVideoUI.
 * 
 * @author mstevens, Michalis Vitos, benelliott
 *
 * @param <MF>
 */
public abstract class AndroidCameraUI<MF extends MediaField> extends AndroidMediaUI<MF>
{

	protected CameraController cameraController;
	private SurfaceView captureSurface;
	
	/**
	 * @see AndroidMediaUI#AndroidMediaUI(MediaField, AndroidCollectorController, CollectorView, boolean)
	 */
	public AndroidCameraUI(MF field, AndroidCollectorController controller, CollectorView collectorUI, boolean unblockUIAfterCaptureClick)
	{
		super(field, controller, collectorUI, unblockUIAfterCaptureClick);
	}
	
	protected void initCameraController(boolean forVideoRecording, boolean frontFacing, FlashMode flashMode)
	{
		if(cameraController == null)
		{
			// Set up camera controller & camera selection:
			cameraController = new CameraController(forVideoRecording);
			cameraController.setCamera(frontFacing);
			if(!cameraController.foundCamera())
			{ 	// No camera found, try the other one:
				cameraController.setCamera(!frontFacing);
				if(!cameraController.foundCamera())
				{	// Still no camera, this device does not seem to have one:
					cameraController = null; // make cameraController null!
					return;
				}
			}
			// Set flash mode:
			cameraController.setFlashMode(flashMode);
			// TODO Other camera config?
		}
	}
	
	@SuppressWarnings("deprecation")
	protected View getCaptureContent(Context context, boolean forVideoRecording, boolean frontFacing, FlashMode flashMode)
	{
		// Initialise camera:
		initCameraController(forVideoRecording, frontFacing, flashMode);
		
		if(cameraController != null)
		{
			// Create the surface for previewing the camera:
			if(captureSurface == null)
			{
				captureSurface = new SurfaceView(context);	
				// Set-up surface holder:
				SurfaceHolder holder = captureSurface.getHolder();
				holder.addCallback(cameraController);
				holder.setKeepScreenOn(true);
				// !!! Deprecated but cameraController preview crashes without it (at least on the XCover/Gingerbread):
				holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
			}
			cameraController.startPreview();
			return captureSurface;
		}
		else
		{
			return	new DrawableItem(new SaltireCross(Color.parseColor(ChoiceField.DEFAULT_CROSS_COLOR), AndroidChoiceUI.CROSS_THICKNESS))
					.setBackgroundColor(Color.TRANSPARENT)
					.getView(context);
		}
	}
	
	@Override
	protected void onCapture()
	{
		if(cameraController == null)
			handleCaptureError(new IllegalStateException("No camera set/found"));
		else
			doCapture();
	}
	
	/**
	 * When this method is called the cameraController can be assumed to be non-null and having found a camera
	 */
	protected abstract void doCapture();
	
	@Override
	protected void cancel()
	{
		super.cancel();
		if(cameraController != null)
			cameraController.close();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#informOnDisplayNonAudioFeedback(boolean)
	 */
	@Override
	protected boolean informOnDisplayNonAudioFeedback(boolean withPage)
	{
		if(!withPage && isInCaptureMode() && cameraController == null)
			return true;
		else
			return super.informOnDisplayNonAudioFeedback(withPage);
	}

	/**
	 * Used for error handling: detect cases whether cameraController could not be initialised.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#onDisplayNonAudioFeedback(boolean)
	 */
	@Override
	protected void onDisplayNonAudioFeedback(boolean withPage)
	{
		if(!withPage && isInCaptureMode() && cameraController == null)
		{
			attachMedia(null);
			if(isValid(controller.getCurrentRecord()))
			{	// Non-fatal (photo/video is optional):
				String errorMsg = collectorUI.getContext().getString(getCameraErrorStringId(false), field.id);
				if(wasUserGoingBack())
				{
					Toast.makeText(collectorUI.getContext(), errorMsg, Toast.LENGTH_LONG).show();
					controller.goBack(false);
				}
				else
				{
					collectorUI.activity.showOKDialog(R.string.error, errorMsg, false, new Runnable()
					{
						@Override
						public void run()
						{
							controller.goForward(false);
						}
					});
				}	
			}	
			else
			{	// Fatal (photo/video is mandatory):
				collectorUI.activity.showErrorDialog(getCameraErrorStringId(true), true);
			}
		}
		else
			super.onDisplayNonAudioFeedback(withPage);
	}
	
	protected abstract int getCameraErrorStringId(boolean fatal);

}
