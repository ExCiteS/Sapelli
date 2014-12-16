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

package uk.ac.ucl.excites.sapelli.collector.model;

import java.io.File;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;

/**
 * @author mstevens
 *
 */
public class Control
{

	// Statics-------------------------------------------------------
	static public enum Type
	{
		Back,
		//Up,
		Cancel,
		Forward,
		//Exit,
	}
	
	static public String GetDefaultDescriptionText(Type type)
	{
		return type.name(); // TODO this may change in the future (e.g. full sentence, or string-id for multi-language string lookup)
	}
	
	static public final String DEFAULT_BACKGROUND_COLOR = Form.DEFAULT_CONTROL_BACKGROUND_COLOR; // light gray
	
	// Dynamics------------------------------------------------------
	public final Form form;
	public final Type type;
	private String imageRelativePath;
	private String backgroundColor;
	public final Description description;

	/**
	 * @param type
	 */
	public Control(Form form, Type type)
	{
		if(form == null)
			throw new NullPointerException("Form cannot be null!");
		if(type == null)
			throw new NullPointerException("Control type cannot be null!");
		this.form = form;
		this.type = type;
		this.description = new Description();
		
		// Default values:
		backgroundColor = DEFAULT_BACKGROUND_COLOR;
		description.setText(GetDefaultDescriptionText(type));
	}

	/**
	 * @return the backgroundColor
	 */
	public String getBackgroundColor()
	{
		return backgroundColor;
	}

	/**
	 * @param backgroundColor the backgroundColor to set
	 */
	public void setBackgroundColor(String backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}

	/**
	 * @return the imageRelativePath
	 */
	public String getImageRelativePath()
	{
		return imageRelativePath;
	}

	/**
	 * @param imageRelativePath the imageRelativePath to set
	 */
	public void setImageRelativePath(String imageRelativePath)
	{
		this.imageRelativePath = imageRelativePath;
	}
	
	/**
	 * @param filesSet set to add files to
	 * @param fileStorageProvider to resolve relative paths
	 */
	public void addFiles(Set<File> filesSet, FileStorageProvider fileStorageProvider)
	{
		// img:
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, imageRelativePath));
		// Audio description:
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectSoundFile(form.project, description.getAudioRelativePath()));
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof Control)
		{
			Control that = (Control) obj;
			return	this.type == that.type &&
					(this.imageRelativePath != null ? this.imageRelativePath.equals(that.imageRelativePath) : that.imageRelativePath == null) &&
					(this.backgroundColor != null ? this.backgroundColor.equals(that.backgroundColor) : that.backgroundColor == null) &&
					this.description.equals(that.description);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + type.ordinal();
		hash = 31 * hash + (imageRelativePath == null ? 0 : imageRelativePath.hashCode());
		hash = 31 * hash + (backgroundColor == null ? 0 : backgroundColor.hashCode());
		hash = 31 * hash + description.hashCode();
		return hash;
	}
	
}
