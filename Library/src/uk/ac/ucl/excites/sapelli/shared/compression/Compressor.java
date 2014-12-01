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

package uk.ac.ucl.excites.sapelli.shared.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;

/**
 * @author mstevens
 * 
 */
public abstract class Compressor
{

	public abstract CompressorFactory.Compression getMode();

	public abstract OutputStream getOutputStream(OutputStream sink) throws IOException;

	/**
	 * @param data
	 * @return
	 * @throws IOException
	 */
	public byte[] compress(byte[] data) throws IOException
	{
		ByteArrayOutputStream byteArraySink = new ByteArrayOutputStream();
		OutputStream out = null;
		try
		{
			out = getOutputStream(byteArraySink);
			out.write(data);
			out.flush();
			out.close();
			return byteArraySink.toByteArray();
		}
		catch(IOException ioe)
		{
			throw new IOException("Error upon " + getMode() + " compression", ioe);
		}
		finally
		{
			try
			{
				if(out != null)
					out.close();
			}
			catch(Exception ignore)
			{
			}
		}
	}

	public abstract InputStream getInputStream(InputStream source) throws IOException;

	/**
	 * @param compressedData
	 * @return
	 * @throws IOException
	 */
	public byte[] decompress(byte[] compressedData) throws IOException
	{
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		InputStream in = null;
		try
		{
			in = getInputStream(new ByteArrayInputStream(compressedData));
			IOUtils.copy(in, out);
			in.close();
			out.flush();
			return out.toByteArray();
		}
		catch(IOException ioe)
		{
			throw new IOException("Error upon " + getMode() + " decompression", ioe);
		}
		finally
		{
			try
			{
				if(out != null)
					out.close();
			}
			catch(Exception ignore)
			{
			}
			try
			{
				if(in != null)
					in.close();
			}
			catch(Exception ignore)
			{
			}
		}
	}

	// public class CompressorCallable implements Callable<CompressorResult>
	// {
	//
	// private byte[] uncompressedData;
	//
	// public CompressorCallable(byte[] uncompressedData)
	// {
	// this.uncompressedData = uncompressedData;
	// }
	//
	// @Override
	// public CompressorResult call() throws Exception
	// {
	// return new CompressorResult(getMode(), com, ratio)
	// }
	//
	// }

}
