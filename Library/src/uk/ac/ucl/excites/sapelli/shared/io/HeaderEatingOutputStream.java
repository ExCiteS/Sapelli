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

package uk.ac.ucl.excites.sapelli.shared.io;

import java.io.IOException;
import java.io.OutputStream;

/**
 * OutputStream wrapper which eats the first {@link #bytesToEat} bytes (to strip off a header)
 * 
 * @author mstevens
 */
public class HeaderEatingOutputStream extends OutputStream
{

	private final OutputStream out;
	private final int bytesToEat;
	private int bytesEaten = 0;
	
	/**
	 * @param out
	 * @param bytesToEat
	 */
	public HeaderEatingOutputStream(OutputStream out, int bytesToEat)
	{
		if(out == null)
			throw new NullPointerException("OutputStream cannot be null!");
		if(bytesToEat < 0)
			throw new IllegalArgumentException("bytesToEat must be >= 0");
		this.out = out;
		this.bytesToEat = bytesToEat;
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#write(int)
	 */
	@Override
	public void write(int b) throws IOException
	{
		if(bytesEaten < bytesToEat)
			bytesEaten++;
		else
			out.write(b);
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#flush()
	 */
	@Override
	public void flush() throws IOException
	{
		out.flush();
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#close()
	 */
	@Override
	public void close() throws IOException
	{
		out.close();
	}

}
