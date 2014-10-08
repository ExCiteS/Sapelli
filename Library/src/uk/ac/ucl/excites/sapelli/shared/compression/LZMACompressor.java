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
import java.io.SequenceInputStream;

import lzma.sdk.lzma.Decoder;
import lzma.sdk.lzma.Encoder;
import lzma.streams.LzmaInputStream;
import lzma.streams.LzmaOutputStream;
import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory.Compression;
import uk.ac.ucl.excites.sapelli.shared.io.HeaderEatingOutputStream;

/**
 * LZMA compressor.<br/>
 * Uses the lzma-java library by Julien Ponge (jponge) et al. (Apache License 2.0).
 * 
 * @author mstevens
 * @see <a href="http://en.wikipedia.org/wiki/LZMA">http://en.wikipedia.org/wiki/LZMA</a>
 * @see <a href="https://github.com/jponge/lzma-java">https://github.com/jponge/lzma-java</a>
 */
public class LZMACompressor extends Compressor
{
	
	static public final boolean DEFAULT_HEADERLESS = true;
	static private final int HEADER_SIZE = 5 /*encoder properties*/ + 8 /*uncompressed size*/; // bytes
	
	private final boolean headerless;

	public LZMACompressor()
	{
		this(DEFAULT_HEADERLESS);
	}

	public LZMACompressor(boolean headerless)
	{
		this.headerless = headerless;
	}
	
	public Encoder getEncoder()
	{
		Encoder encoder = new Encoder();

		// Properties:
		encoder.setDictionarySize(1 << 20); // Default: 1 << 23 (way too much memory usage!)
		encoder.setEndMarkerMode(true); // takes up 5 bytes, but without it decoding fails to end!
		encoder.setMatchFinder(Encoder.EMatchFinderTypeBT4);
		encoder.setNumFastBytes(0x20); // Default: 0x20 (= 32)
		
		return encoder;
	}
	
	@SuppressWarnings("resource")
	@Override
	public OutputStream getOutputStream(OutputStream sink) throws IOException
	{
		return new LzmaOutputStream(headerless ? new HeaderEatingOutputStream(sink, HEADER_SIZE) : sink, getEncoder());
	}

	@Override
	public InputStream getInputStream(InputStream source) throws IOException
	{
		// Let encoder write out our default properties:
		
		if(headerless)
		{
			// Generate fake header:
			ByteArrayOutputStream headerOut = new ByteArrayOutputStream();
			// Write header with coder settings (takes up 5 bytes)
			getEncoder().writeCoderProperties(headerOut);
			// write -1 as "unknown" for file size (takes up 8 bytes)
			for(int i = 0; i < 8; ++i)
				headerOut.write((byte) -1);
			headerOut.close();
			
			// Insert header:
			return new LzmaInputStream(new SequenceInputStream(new ByteArrayInputStream(headerOut.toByteArray()), source), new Decoder());
		}
		else
			return new LzmaInputStream(source, new Decoder());
	}
	
	@Override
	public Compression getMode()
	{
		return Compression.LZMA;
	}

}
