/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class MediaFile extends Attachment<MediaField>
{
	
	public final long creationTimeOffset;
	
	/**
	 * @param field
	 * @param record
	 * @param file
	 */
	public MediaFile(MediaField field, Record record, long creationTimeOffset, File file)
	{
		super(field, record, file);
		this.creationTimeOffset = creationTimeOffset;
	}
	
	@Override
	public String getMimeType()
	{
		return field.getFileMimeType();
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof MediaFile)
		{
			MediaFile that = (MediaFile) obj;
			return	super.equals(that) && // Attachment#equals(Object)
					this.creationTimeOffset == that.creationTimeOffset;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Attachment#hashCode()
		hash = 31 * hash + (int)(creationTimeOffset ^ (creationTimeOffset >>> 32));
		return hash;
	}
	
}
