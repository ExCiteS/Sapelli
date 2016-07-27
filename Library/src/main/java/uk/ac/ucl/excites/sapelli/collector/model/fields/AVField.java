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

import java.io.File;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;

/**
 * @author mstevens
 *
 */
public abstract class AVField extends MediaField
{

	protected String startRecImageRelativePath;
	protected String stopRecImageRelativePath;
	
	protected String startPlaybackImageRelativePath;
	protected String pausePlaybackImageRelativePath;
	protected String stopPlaybackImageRelativePath;

	public AVField(Form form, String id, String caption, boolean useNativeApp)
	{
		super(form, id, caption, useNativeApp);
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
	 * @return the stopRecImageRelativePath
	 */
	public String getStopRecImageRelativePath()
	{
		return stopRecImageRelativePath;
	}

	/**
	 * @param stopRecImageRelativePath the stopRecImageRelativePath to set
	 */
	public void setStopRecImageRelativePath(String stopRecImageRelativePath)
	{
		this.stopRecImageRelativePath = stopRecImageRelativePath;
	}
	
	/**
	 * @return the startPlaybackImageRelativePath
	 */
	public String getStartPlaybackImageRelativePath()
	{
		return startPlaybackImageRelativePath;
	}

	/**
	 * @param startPlaybackImageRelativePath the startPlaybackImageRelativePath to set
	 */
	public void setStartPlaybackImageRelativePath(String startPlaybackImageRelativePath)
	{
		this.startPlaybackImageRelativePath = startPlaybackImageRelativePath;
	}

	/**
	 * @return the pausePlaybackImageRelativePath
	 */
	public String getPausePlaybackImageRelativePath()
	{
		return pausePlaybackImageRelativePath;
	}

	/**
	 * @param pausePlaybackImageRelativePath the pausePlaybackImageRelativePath to set
	 */
	public void setPausePlaybackImageRelativePath(String pausePlaybackImageRelativePath)
	{
		this.pausePlaybackImageRelativePath = pausePlaybackImageRelativePath;
	}

	/**
	 * @return the stopPlaybackImageRelativePath
	 */
	public String getStopPlaybackImageRelativePath()
	{
		return stopPlaybackImageRelativePath;
	}

	/**
	 * @param stopPlaybackImageRelativePath the stopPlaybackImageRelativePath to set
	 */
	public void setStopPlaybackImageRelativePath(String stopPlaybackImageRelativePath)
	{
		this.stopPlaybackImageRelativePath = stopPlaybackImageRelativePath;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#addFiles(java.util.Set, uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider)
	 */
	@Override
	public void addFiles(Set<File> filesSet, FileStorageProvider fileStorageProvider)
	{
		super.addFiles(filesSet, fileStorageProvider); // !!!
		
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, startRecImageRelativePath));
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, stopRecImageRelativePath));
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, startPlaybackImageRelativePath));
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, pausePlaybackImageRelativePath));
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, stopPlaybackImageRelativePath));
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof AudioField)
		{
			AVField that = (AVField) obj;
			return	super.equals(that) && // MediaField#equals(Object)
					Objects.equals(this.startRecImageRelativePath,	that.startRecImageRelativePath) &&
					Objects.equals(this.stopRecImageRelativePath,	that.stopRecImageRelativePath) &&
					Objects.equals(this.startPlaybackImageRelativePath,	that.startPlaybackImageRelativePath) &&
					Objects.equals(this.pausePlaybackImageRelativePath,	that.pausePlaybackImageRelativePath) &&
					Objects.equals(this.stopPlaybackImageRelativePath,	that.stopPlaybackImageRelativePath);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // MediaField#hashCode()
		hash = 31 * hash + Objects.hashCode(startRecImageRelativePath);
		hash = 31 * hash + Objects.hashCode(stopRecImageRelativePath);
		hash = 31 * hash + Objects.hashCode(startPlaybackImageRelativePath);
		hash = 31 * hash + Objects.hashCode(pausePlaybackImageRelativePath);
		hash = 31 * hash + Objects.hashCode(stopPlaybackImageRelativePath);
		return hash;
	}

}
