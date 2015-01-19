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

import java.io.File;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.MediaUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;

/**
 * @author Michalis Vitos, mstevens, benelliott
 *
 */
public class VideoField extends MediaField
{

	// STATICS--------------------------------------------------------
	static private final String MEDIA_TYPE_MP4 = "VIDEO_MP4";
	static private final String EXTENSION_MP4 = "mp4";

	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	static public final boolean DEFAULT_USE_FRONT_FACING_CAMERA = false;

	// DYNAMICS-------------------------------------------------------
	private String startRecImageRelativePath;
	private String stopRecImageRelativePath;
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
	 * @param useFrontFacingCamera
	 *            the useFrontFacingCamera to set
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
	public void setStopRecImageRelativePath(String stopRecImageRelativePath)
	{
		this.stopRecImageRelativePath = stopRecImageRelativePath;
	}

	@Override
	public void addFiles(Set<File> filesSet, FileStorageProvider fileStorageProvider)
	{
		super.addFiles(filesSet, fileStorageProvider); // !!!
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, startRecImageRelativePath));
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, stopRecImageRelativePath));
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
			return super.equals(that) && // MediaField#equals(Object)
					(this.startRecImageRelativePath != null ? this.startRecImageRelativePath.equals(that.startRecImageRelativePath)
							: that.startRecImageRelativePath == null) &&
					(this.stopRecImageRelativePath != null ? this.stopRecImageRelativePath.equals(that.stopRecImageRelativePath)
							: that.stopRecImageRelativePath == null);
		}
		else
			return false;
	}

	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // MediaField#hashCode()
		hash = 31 * hash + (startRecImageRelativePath == null ? 0 : startRecImageRelativePath.hashCode());
		hash = 31 * hash + (stopRecImageRelativePath == null ? 0 : stopRecImageRelativePath.hashCode());
		return hash;
	}
	
}
