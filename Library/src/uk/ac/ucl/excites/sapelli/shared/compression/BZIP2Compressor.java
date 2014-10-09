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
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.SequenceInputStream;

import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream;

import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory.Compression;
import uk.ac.ucl.excites.sapelli.shared.io.HeaderEatingOutputStream;

/**
 * BZIP2 compressor.
 * Based on Commons Compress.
 * When {@link #headerless} is {@code true} the 3-byte "BZh" header will be stripped off compression result and will *not* be expected on decompression input.
 * 
 * @author mstevens
 * @see <a href="http://en.wikipedia.org/wiki/Bzip2">http://en.wikipedia.org/wiki/Bzip2</a>
 * @see <a href="http://commons.apache.org/proper/commons-compress">http://commons.apache.org/proper/commons-compress</a>
 */
public class BZIP2Compressor extends Compressor
{
	
	static private byte[] BZIP2_HEADER = { 0x42, 0x5A, 0x68 }; // = "BZh"
	static public final boolean DEFAULT_HEADERLESS = true;
	
	private final boolean headerless;
	
	/**
	 * 
	 */
	public BZIP2Compressor()
	{
		this(DEFAULT_HEADERLESS);
	}
	
	/**
	 * @param headerless
	 */
	public BZIP2Compressor(boolean headerless)
	{
		this.headerless = headerless;
	}

	@SuppressWarnings("resource")
	@Override
	public OutputStream getOutputStream(OutputStream sink) throws IOException
	{
		return new BZip2CompressorOutputStream(headerless ? new HeaderEatingOutputStream(sink, BZIP2_HEADER.length) : sink);
	}

	@Override
	public InputStream getInputStream(InputStream source) throws IOException
	{
		return new BZip2CompressorInputStream(headerless ? new SequenceInputStream(new ByteArrayInputStream(BZIP2_HEADER), source) : source);
	}
	
	@Override
	public Compression getMode()
	{
		return Compression.BZIP2;
	}

}
