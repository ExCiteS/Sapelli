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

package uk.ac.ucl.excites.sapelli.shared.util.android.http;

import java.io.File;
import java.io.FileNotFoundException;

import com.loopj.android.http.RequestParams;

import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;

/**
 * @author mstevens
 *
 */
public class ChainableRequestParams extends RequestParams
{

	private static final long serialVersionUID = -7816707079213769847L;

	/**
	 * @param key
	 * @param value
	 * @return
	 * 
	 * @see com.loopj.android.http.RequestParams#put(java.lang.String, java.lang.Object)
	 */
	public ChainableRequestParams putt(String key, Object value)
	{
		super.put(key, value);
		return this;
	}
	
	/* (non-Javadoc)
	 * @see com.loopj.android.http.RequestParams#put(java.lang.String, java.io.File)
	 */
	public ChainableRequestParams putt(String key, File file)
	{
		return putt(key, file, null);
	}
	
	/* (non-Javadoc)
	 * @see com.loopj.android.http.RequestParams#put(java.lang.String, java.io.File, String)
	 */
	public ChainableRequestParams putt(String key, File file, String contentType)
	{
		if(!FileHelpers.isReadableFile(file))
			return this;
		try
		{
			super.put(key, file, contentType);
		}
		catch(FileNotFoundException fnfe) { /*should never happen*/ }
		return this;
	}
	
	// TODO add the other methods

}
