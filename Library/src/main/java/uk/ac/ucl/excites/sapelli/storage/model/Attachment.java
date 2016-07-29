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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.File;

import org.apache.commons.io.FileUtils;

/**
 * A class which associates a {@link Record} with a {@link File) (which may or may not actually exist).
 * 
 * @author mstevens
 */
public abstract class Attachment
{
	
	public final Record record;
	public final File file;
	
	/**
	 * @param record should not be {@code null}
	 * @param file should not be {@code null}, but may or may not actually exist
	 */
	public Attachment(Record record, File file)
	{
		if(record == null || file == null)
			throw new NullPointerException("Record & file cannot be null!");
		this.record = record;
		this.file = file;
	}
	
	/**
	 * Quietly deletes the file.
	 */
	public void delete()
	{
		FileUtils.deleteQuietly(file);
	}
	
	/**
	 * @return the MIME type of the file
	 */
	public abstract String getMimeType();

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(this.getClass().isInstance(obj))
		{
			Attachment that = (Attachment) obj;
			return	this.record.equals(that.record) &&
					this.file.equals(that.file);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + record.hashCode();
		hash = 31 * hash + file.hashCode();
		return hash;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString()
	{
		return getClass().getSimpleName() + ":" + file.getAbsolutePath();
	}

}
