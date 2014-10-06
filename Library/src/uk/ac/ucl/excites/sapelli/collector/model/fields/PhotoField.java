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

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.MediaUI;

/**
 * @author Michalis Vitos, mstevens
 *
 */
public class PhotoField extends MediaField
{

	//STATICS--------------------------------------------------------
	static private final String MEDIA_TYPE_JPEG = "PHOTO_JPEG";
	static private final String EXTENSION_JPEG = "jpg";
	
	static public enum FlashMode
	{
		AUTO,
		ON,
		OFF
	}
	
	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	static public final boolean DEFAULT_USE_FRONT_FACING_CAMERA = false;
	static public final FlashMode DEFAULT_FLASH_MODE = FlashMode.AUTO;
	
	//DYNAMICS-------------------------------------------------------
	private boolean useFrontFacingCamera;
	private FlashMode flashMode;
	

	
	public PhotoField(Form form, String id, boolean multiple, String caption)
	{
		super(form, id, multiple, caption);
		useNativeApp = DEFAULT_USE_NATIVE_APP;
		useFrontFacingCamera = DEFAULT_USE_FRONT_FACING_CAMERA;
		flashMode = DEFAULT_FLASH_MODE;
	}

	/**
	 * @return the useFrontFacingCamera
	 */
	public boolean isUseFrontFacingCamera()
	{
		return useFrontFacingCamera;
	}

	/**
	 * @param useFrontFacingCamera the useFrontFacingCamera to set
	 */
	public void setUseFrontFacingCamera(boolean useFrontFacingCamera)
	{
		this.useFrontFacingCamera = useFrontFacingCamera;
	}

	/**
	 * @return the flashMode
	 */
	public FlashMode getFlashMode()
	{
		return flashMode;
	}

	/**
	 * @param flashMode the flashMode to set
	 */
	public void setFlashMode(FlashMode flashMode)
	{
		this.flashMode = flashMode;
	}



	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_JPEG;
	}

	@Override
	protected String getFileExtension(String mediaType)
	{
		return EXTENSION_JPEG;
	}
	
	@Override
	public <V, UI extends CollectorUI<V, UI>> MediaUI<PhotoField,V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createPhotoUI(this);
	}
	
}
