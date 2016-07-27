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

package uk.ac.ucl.excites.sapelli.shared.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;

import uk.ac.ucl.excites.sapelli.shared.io.StreamHelpers;

/**
 * @author mstevens
 * 
 */
public abstract class Compressor
{
	
	static public final long UNKNOWN_UNCOMPRESSED_SIZE = -1;

	public abstract CompressorFactory.Compression getMode();

	public final OutputStream getOutputStream(OutputStream sink) throws IOException
	{
		return getOutputStream(sink, UNKNOWN_UNCOMPRESSED_SIZE);
	}
	
	public OutputStream getOutputStream(OutputStream sink, long uncompressedSizeBytes) throws IOException, IllegalArgumentException
	{
		if(uncompressedSizeBytes < UNKNOWN_UNCOMPRESSED_SIZE)
			throw new IllegalArgumentException("Invalid uncompressed size (must be >= 0 when known or == -1 when unknown).");
		return _getOutputStream(sink, uncompressedSizeBytes);
	}
	
	protected abstract OutputStream _getOutputStream(OutputStream sink, long uncompressedSizeBytes) throws IOException;
	
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
			out = getOutputStream(byteArraySink, data.length);
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
			StreamHelpers.SilentClose(out);
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
			StreamHelpers.SilentClose(out);
			StreamHelpers.SilentClose(in);
		}
	}
	
	@Override
	public String toString()
	{
		return getMode().name() + Compressor.class.getSimpleName();
	}

	/*public class CompressorCallable implements Callable<CompressorResult>
	{

		private byte[] uncompressedData;

		public CompressorCallable(byte[] uncompressedData)
		{
			this.uncompressedData = uncompressedData;
		}

		@Override
		 public CompressorResult call() throws Exception
		 {
		 return new CompressorResult(getMode(), com, ratio)
		 }

	}*/

}
