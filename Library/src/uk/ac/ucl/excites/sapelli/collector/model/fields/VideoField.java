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
 * @author Michalis Vitos, mstevens, benelliott
 *
 */
public class VideoField extends MediaField
{

	//STATICS--------------------------------------------------------
	static private final String MEDIA_TYPE_MP4 = "VIDEO_MP4";
	static private final String EXTENSION_MP4 = "mp4";
	
	static public enum FlashMode
	{
		AUTO,
		ON,
		OFF
	}
	
	private String startRecImageRelativePath;
	private String stopRecImageRelativePath;
	
	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	static public final boolean DEFAULT_USE_FRONT_FACING_CAMERA = false;
	
	//DYNAMICS-------------------------------------------------------
	private boolean useFrontFacingCamera;
	

	
	public VideoField(Form form, String id, String caption)
	{
		super(form, id, caption);
		useNativeApp = DEFAULT_USE_NATIVE_APP;
		useFrontFacingCamera = DEFAULT_USE_FRONT_FACING_CAMERA;
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

	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_MP4;
	}

	@Override
	protected String getFileExtension(String mediaType)
	{
		return EXTENSION_MP4;
	}
	
	/**
	 * @return the startRecImageRelativePath
	 */
	public String getStartRecImageRelativePath()
	{
		return startRecImageRelativePath;
	}

	/**
	 * @param startRecImageRelativePath the startRecImageRelativePath to set
	 */
	public void setStartRecImageRelativePath(String startRecImageRelativePath)
	{
		this.startRecImageRelativePath = startRecImageRelativePath;
	}

	/**
	 * @return the stopVideoImageRelativePath
	 */
	public String getStopRecImageRelativePath()
	{
		return stopRecImageRelativePath;
	}

	/**
	 * @param stopVideoImageRelativePath the stopVideoImageRelativePath to set
	 */
	public void setStopRecAudioImageRelativePath(String stopRecImageRelativePath)
	{
		this.stopRecImageRelativePath = stopRecImageRelativePath;
	}

	
	@Override
	public <V, UI extends CollectorUI<V, UI>> MediaUI<VideoField,V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createVideoUI(this);
	}
}
