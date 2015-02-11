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

import java.io.Serializable;

/**
 * @author mstevens
 *
 */
public class Description implements Serializable
{

	private static final long serialVersionUID = 2L;

	protected String text;
	protected String audioRelativePath;
	
	/**
	 * @return the text
	 */
	public String getText()
	{
		return text;
	}
	
	/**
	 * @param text the text to set
	 */
	public void setText(String text)
	{
		this.text = text;
	}
	
	/**
	 * @return the audioRelativePath
	 */
	public String getAudioRelativePath()
	{
		return audioRelativePath;
	}

	/**
	 * @param audioRelativePath the audioRelativePath to set
	 */
	public void setAudioRelativePath(String audioRelativePath)
	{
		this.audioRelativePath = audioRelativePath;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof Description)
		{
			Description that = (Description) obj;
			return	(this.text != null ? this.text.equals(that.text) : that.text == null) &&
					(this.audioRelativePath != null ? this.audioRelativePath.equals(that.audioRelativePath) : that.audioRelativePath == null);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (text == null ? 0 : text.hashCode());
		hash = 31 * hash + (audioRelativePath == null ? 0 : audioRelativePath.hashCode());
		return hash;
	}

}