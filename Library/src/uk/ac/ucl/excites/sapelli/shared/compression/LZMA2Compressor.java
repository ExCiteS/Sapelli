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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.tukaani.xz.FinishableOutputStream;
import org.tukaani.xz.LZMA2Options;
import org.tukaani.xz.UnsupportedOptionsException;

import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory.Compression;

/**
 * LZMA2 compressor.
 * Uses the "XZ for Java" library (public domain).
 * 
 * @author mstevens
 * @see <a href="http://en.wikipedia.org/wiki/LZMA#LZMA2_format">http://en.wikipedia.org/wiki/LZMA#LZMA2_format</a>
 * @see <a href="http://tukaani.org/xz/java.html">http://tukaani.org/xz/java.html</a>
 */
public class LZMA2Compressor extends Compressor
{

	private LZMA2Options options;
	
	public LZMA2Compressor()
	{
		try
		{
			options = new LZMA2Options(6);
			options.setDictSize(1 << 20); // dictSize at present 6 is too high
			//options.setNiceLen(LZMA2Options.NICE_LEN_MAX); // = "NumFastBytes" on LZMA(1)
		}
		catch(UnsupportedOptionsException e)
		{
			e.printStackTrace();
		}
	}
	
	@Override
	public OutputStream getOutputStream(OutputStream sink)
	{
		return options.getOutputStream(new WrappedOutputStream(sink));
	}
	
	@Override
	public InputStream getInputStream(InputStream source) throws IOException
	{
		return options.getInputStream(source);
	}

	public class WrappedOutputStream extends FinishableOutputStream
	{
		
		private OutputStream wrappedStream;
		
		public WrappedOutputStream(OutputStream wrappedStream)
		{
			this.wrappedStream = wrappedStream;
		}

		@Override
		public void write(int b) throws IOException
		{
			wrappedStream.write(b);
		}

		/* (non-Javadoc)
		 * @see java.io.OutputStream#flush()
		 */
		@Override
		public void flush() throws IOException
		{
			wrappedStream.flush();
		}

		/* (non-Javadoc)
		 * @see java.io.OutputStream#close()
		 */
		@Override
		public void close() throws IOException
		{
			wrappedStream.close();
		}

	}

	@Override
	public Compression getMode()
	{
		return Compression.LZMA2;
	}

}
