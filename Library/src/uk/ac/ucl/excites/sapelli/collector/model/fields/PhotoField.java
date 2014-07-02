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
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.PhotoUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;

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
	
	private String captureButtonImageRelativePath;
	private String approveButtonImageRelativePath;
	private String discardButtonImageRelativePath;
	
	public PhotoField(Form form, String id, String caption)
	{
		super(form, id, caption);
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

	/**
	 * @return the captureButtonImageRelativePath
	 */
	public String getCaptureButtonImageRelativePath()
	{
		return captureButtonImageRelativePath;
	}

	/**
	 * @param captureButtonImageRelativePath the captureButtonImageRelativePath to set
	 */
	public void setCaptureButtonImageRelativePath(String captureButtonImageRelativePath)
	{
		this.captureButtonImageRelativePath = captureButtonImageRelativePath;
	}

	/**
	 * @return the approveButtonImageRelativePath
	 */
	public String getApproveButtonImageRelativePath()
	{
		return approveButtonImageRelativePath;
	}

	/**
	 * @param approveButtonImageRelativePath the approveButtonImageRelativePath to set
	 */
	public void setApproveButtonImageRelativePath(String approveButtonImageRelativePath)
	{
		this.approveButtonImageRelativePath = approveButtonImageRelativePath;
	}

	/**
	 * @return the discardButtonImageRelativePath
	 */
	public String getDiscardButtonImageRelativePath()
	{
		return discardButtonImageRelativePath;
	}

	/**
	 * @param discardButtonImageRelativePath the discardButtonImageRelativePath to set
	 */
	public void setDiscardButtonImageRelativePath(String discardButtonImageRelativePath)
	{
		this.discardButtonImageRelativePath = discardButtonImageRelativePath;
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
	public List<File> getFiles(Project project)
	{
		List<File> paths = new ArrayList<File>();
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(captureButtonImageRelativePath));
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(approveButtonImageRelativePath));
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(discardButtonImageRelativePath));
		return paths;
	}
	
	@Override
	public <V, UI extends CollectorUI<V, UI>> PhotoUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createPhotoUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof PhotoField)
		{
			PhotoField that = (PhotoField) obj;
			return	super.equals(that) && // MediaField#equals(Object)
					this.useFrontFacingCamera == that.useFrontFacingCamera &&
					this.flashMode == that.flashMode &&
					(this.captureButtonImageRelativePath != null ? this.captureButtonImageRelativePath.equals(that.captureButtonImageRelativePath) : that.captureButtonImageRelativePath == null) &&
					(this.approveButtonImageRelativePath != null ? this.approveButtonImageRelativePath.equals(that.approveButtonImageRelativePath) : that.approveButtonImageRelativePath == null) &&
					(this.discardButtonImageRelativePath != null ? this.discardButtonImageRelativePath.equals(that.discardButtonImageRelativePath) : that.discardButtonImageRelativePath == null);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // MediaField#hashCode()
		hash = 31 * hash + (useFrontFacingCamera ? 0 : 1);
		hash = 31 * hash + flashMode.ordinal();
		hash = 31 * hash + (captureButtonImageRelativePath == null ? 0 : captureButtonImageRelativePath.hashCode());
		hash = 31 * hash + (approveButtonImageRelativePath == null ? 0 : approveButtonImageRelativePath.hashCode());
		hash = 31 * hash + (discardButtonImageRelativePath == null ? 0 : discardButtonImageRelativePath.hashCode());		
		return hash;
	}
	
}
