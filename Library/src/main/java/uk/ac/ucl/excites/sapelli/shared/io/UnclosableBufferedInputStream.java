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

package uk.ac.ucl.excites.sapelli.shared.io;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author mstevens
 *
 */
public class UnclosableBufferedInputStream extends BufferedInputStream
{
	
	private boolean allowClose = false;
	
	public UnclosableBufferedInputStream(InputStream in)
	{
		super(in);
	}

	public UnclosableBufferedInputStream(InputStream in, int size)
	{
		super(in, size);
	}
	
	public void makeClosable()
	{
		allowClose = true;
	}

	public void close() throws IOException
	{
		if(allowClose)
			forceClose();
		//else: do nothing
	}
	
	public void forceClose() throws IOException
	{
		super.close();
	}

}
