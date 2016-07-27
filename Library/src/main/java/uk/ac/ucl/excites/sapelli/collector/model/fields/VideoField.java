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

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.MediaUI;

/**
 * @author Michalis Vitos, mstevens, benelliott
 *
 */
public class VideoField extends AVField
{

	// STATICS-------------------------------------------------------
	static public final String MEDIA_TYPE_MP4 = "VIDEO_MP4";
	static public final String EXTENSION_MP4 = "mp4";
	static public final String MIME_TYPE_MP4 = "video/mp4";

	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	static public final boolean DEFAULT_USE_FRONT_FACING_CAMERA = false;

	// DYNAMICS------------------------------------------------------
	private boolean useFrontFacingCamera = DEFAULT_USE_FRONT_FACING_CAMERA;

	public VideoField(Form form, String id, String caption)
	{
		super(form, id, caption, DEFAULT_USE_NATIVE_APP);
	}

	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_MP4; //TODO support for other types
	}

	@Override
	protected String getFileExtension(String mediaType)
	{
		if(mediaType == MEDIA_TYPE_MP4)
			return EXTENSION_MP4;
		//else if //...
		else
			return EXTENSION_MP4; //or the default
	}
	
	@Override
	protected String getFileMimeType(String mediaType)
	{
		if(mediaType == MEDIA_TYPE_MP4)
			return MIME_TYPE_MP4;
		//else if //...
		else
			return MIME_TYPE_MP4; //or the default
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
	public <V, UI extends CollectorUI<V, UI>> MediaUI<VideoField, V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createVideoUI(this);
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof VideoField)
		{
			VideoField that = (VideoField) obj;
			return	super.equals(that) && // AVField#equals(Object)
					this.useFrontFacingCamera == that.useFrontFacingCamera;
		}
		else
			return false;
	}

	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // AVField#hashCode()
		hash = 31 * hash + (useFrontFacingCamera ? 0 : 1);
		hash = 31 * hash + getClass().getSimpleName().hashCode();
		return hash;
	}
	
}
