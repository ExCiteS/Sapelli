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
import uk.ac.ucl.excites.sapelli.shared.util.Objects;

/**
 * @author Michalis Vitos, mstevens
 * 
 */
public class AudioField extends AVField
{

	// STATICS-------------------------------------------------------
	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	static private final String MEDIA_TYPE_3GPP = "MEDIA_TYPE_3GPP";
	static private final String EXTENSION_3GPP = "3gp";

	// DYNAMICs------------------------------------------------------
	private String recordingImageRelativePath;
	
	public AudioField(Form form, String id, String caption)
	{
		super(form, id, caption, DEFAULT_USE_NATIVE_APP);
	}

	/**
	 * @return the recordingImageRelativePath
	 */
	public String getRecordingImageRelativePath()
	{
		return recordingImageRelativePath;
	}

	/**
	 * @param recordingImageRelativePath the recordingImageRelativePath to set
	 */
	public void setRecordingImageRelativePath(String recordingImageRelativePath)
	{
		this.recordingImageRelativePath = recordingImageRelativePath;
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
			return	super.equals(that) && // AVField#equals(Object)
					Objects.equals(this.recordingImageRelativePath, that.recordingImageRelativePath);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // AVField#hashCode()
		hash = 31 * hash + Objects.hashCode(recordingImageRelativePath);
		return hash;
	}

}
