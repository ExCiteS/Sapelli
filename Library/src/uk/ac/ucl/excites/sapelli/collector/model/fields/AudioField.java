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

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.MediaUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;

/**
 * @author Michalis Vitos, mstevens
 * 
 */
public class AudioField extends MediaField
{

	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	static private final String MEDIA_TYPE_3GPP = "MEDIA_TYPE_3GPP";
	static private final String EXTENSION_3GPP = "3gp";

	private String startRecImageRelativePath;
	private String stopRecImageRelativePath;
	private String playAudioImageRelativePath;
	private String stopAudioImageRelativePath;

	public AudioField(Form form, String id, String caption)
	{
		super(form, id, caption);
		useNativeApp = DEFAULT_USE_NATIVE_APP;
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
	 * @return the playAudioImageRelativePath
	 */
	public String getPlayAudioImageRelativePath()
	{
		return playAudioImageRelativePath;
	}

	/**
	 * @param playAudioImageRelativePath the playAudioImageRelativePath to set
	 */
	public void setPlayAudioImageRelativePath(String playAudioImageRelativePath)
	{
		this.playAudioImageRelativePath = playAudioImageRelativePath;
	}
	
	/**
	 * @return the stopAudioImageRelativePath
	 */
	public String getStopAudioImageRelativePath()
	{
		return stopAudioImageRelativePath;
	}

	/**
	 * @param stopAudioImageRelativePath the stopAudioImageRelativePath to set
	 */
	public void setStopAudioImageRelativePath(String stopAudioImageRelativePath)
	{
		this.stopAudioImageRelativePath = stopAudioImageRelativePath;
	}



	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_3GPP; //TODO support for other types
	}

	@Override
	protected String getFileExtension(String mediaType)
	{
		if(mediaType == MEDIA_TYPE_3GPP)
			return EXTENSION_3GPP;
		//else if //...
		else
			return EXTENSION_3GPP; //or the default
	}

	@Override
	public <V, UI extends CollectorUI<V, UI>> MediaUI<AudioField,V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createAudioUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof AudioField)
		{
			AudioField that = (AudioField) obj;
			return	super.equals(that) && // MediaField#equals(Object)
					(this.startRecImageRelativePath != null ? this.startRecImageRelativePath.equals(that.startRecImageRelativePath) : that.startRecImageRelativePath == null) &&
					(this.stopRecImageRelativePath != null ? this.stopRecImageRelativePath.equals(that.stopRecImageRelativePath) : that.stopRecImageRelativePath == null);
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
