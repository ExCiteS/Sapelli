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
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.SequenceInputStream;
import java.math.BigInteger;

import org.tukaani.xz.LZMA2Options;
import org.tukaani.xz.LZMAInputStream;
import org.tukaani.xz.LZMAOutputStream;

import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory.Compression;
import uk.ac.ucl.excites.sapelli.shared.io.HeaderEatingOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.BigIntegerUtils;

/**
 * LZMA(1) compressor.<br/>
 * Uses the "XZ for Java" library by Lasse Collin (public domain).<br/>
 * <p>
 * 2015-09-16: 	Lasse Collin, author of the "XZ for Java" library, told me in an e-mail
 * 				conversation that we can save 1 byte on LZMA(1) data streams because the
 * 				first byte (after the header) is meaningless and is ignored by the decoder.
 * 				It does however need to be there upon decoding, but it suffices to simply
 * 				insert a 0x00 byte. To eliminate this in {@link #MODE_MINI_HEADER} and
 * 				{@link #MODE_NO_HEADER} we are pretending this byte is part of the LZMA
 * 				file format header.</p>
 * <p>
 * 2015-09-17:	Switched to "XZ for Java" for decoding because it is over 2.5x quicker than
 * 				the lzma-java library in my benchmarks. Unfortunately the XZ library does not yet
 * 				support LZMA(1) encoding. I have asked the author to consider it and he said he would ;-).</p>
 * <p>
 * 2015-09-18:	Introduced 3 modes:<ul><li>
 * 					- {@link #MODE_SPEC_HEADER}: compliant with the LZMA file format specification;</li><li>
 * 					- {@link #MODE_MINI_HEADER}: most efficient for known uncompressed sizes in range of [1, 65535] bytes;</li><li>
 * 					- {@link #MODE_NO_HEADER}: most efficient for unknown uncompressed sizes.</li></ul></p>
 * <p>
 * 2016-11-28:	Reimplemented compression using v1.6 of the "XZ for Java". Upon request by mstevens this library
 * 				now supports writing raw .lzma streams in addition to reading them [thanks Lasse Collin!].
 *				Hence we no longer need to use the slower lzma-java library by Julien Ponge (jponge) et al.<br/>
 * </p>
 *
 * @author mstevens
 * 
 * @see <a href="http://en.wikipedia.org/wiki/LZMA">http://en.wikipedia.org/wiki/LZMA</a>
 * @see <a href="http://git.tukaani.org/?p=xz.git;a=blob_plain;f=doc/lzma-file-format.txt;hb=HEAD">LZMA file format specification</a>
 * @see <a href="http://tukaani.org/xz/java.html">http://tukaani.org/xz/java.html</a>
 * @see <a href="https://github.com/jponge/lzma-java">https://github.com/jponge/lzma-java</a>
 */
public class LZMACompressor extends Compressor
{
	
	/**
	 * Uses an the 13 bytes header (SPECh) compliant with the LZMA file format specification.
	 * Will use an end-of-payload marker (EoPM) if the uncompressed data size is unknown.
	 * Keeps the meaningless byte (B0) at the beginning of the compressed data.
	 * 
	 * Overhead:
	 * 	- When uncompressed size is known:
	 * 		13 (SPECh) + 1 (B0)					= 14 bytes
	 * 	- When uncompressed size in not known:
	 * 		13 (SPECh) + 1 (B0)	+ 5 or 6 (EoPM)	= 19 or 20 bytes
	 */
	static public final int MODE_SPEC_HEADER = 0;
	
	/**
	 * Uses a 2 bytes header (MINIh) which only contains the uncompressed data size.
	 * 
	 * The uncompressed size is being stored as an unsigned little endian integer (as in the SPECh), BUT
	 * the value is shifted by -1 because uncompressed sizes of 0 are uncommon. As in SPECh the maximum
	 * value (all bytes = 0xFF) represents an UNKNOWN size ("=-1").
	 * Concretely this means the following mapping is performed when writing/reading to the size field:
	 * 	UNCOMPRESSED SIZE			<->		STORED AS (2 byte unsigned little endian)
	 * 		 -1 (=UNKNOWN)			<->		 	65535		(="max", see readSize() method)
	 * 		 0						<->			65535		(treated as unknown, because it is uncommon)
	 * 		 1...65535				<->			0...65534	(shifted by -1)
	 * 		 >65535 (= 64kB - 1B)	<->			65535		(treated as unknown, because too big for MINIh, and uncommon in our use case)
	 * Whenever the uncompressed size is unknown, or treated as such, an end-of-payload marker (EoPM) is used.
	 * Eliminates the meaningless byte at the beginning of the compressed data.
	 * 
	 * Overhead:
	 * 	- When uncompressed size is known and in [1, 65535] bytes:
	 * 		 2 (MINh)	 						=  2 bytes
	 * 	- When uncompressed size is not known, or it is known but =0 or >65535:
	 *		 2 (MINh)			+ 5 or 6 (EoPM)	=  7 or 8 bytes
	 */
	static public final int MODE_MINI_HEADER = 1;
	
	/**
	 * Does not use any header.
	 * Will always use an end-of-payload marker (EoPM), regardless of whether the uncompressed data size is
	 * known at encoding time.
	 * Eliminates the meaningless byte at the beginning of the compressed data.
	 * 
	 * Overhead:
	 *		 					  5 or 6 (EoPM)	=  5 or 6 bytes
	 */
	static public final int MODE_NO_HEADER = 2;
	
	static public final int DEFAULT_MODE = MODE_MINI_HEADER;
	
	static protected final int SPEC_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE = 8; // bytes
	
	/**
	 *  The first (meaningless) byte in raw LZMA streams.
	 */
	static protected final byte[] B0 = { 0x00 };

	static protected final int SPEC_HEADER_SIZE =	5 /*encoder properties (actually consisting of 1 byte for properties and 4 bytes for the dictionary size)*/ +
			SPEC_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE +
			B0.length;
	
	static protected final int MINI_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE = 2; // bytes
	
	/**
	 * @see {@link #MODE_MINI_HEADER}
	 */
	static protected final int MINI_HEADER_MIN_KNOWN_UNCOMPRESSED_SIZE = 1;
	
	/**
	 * = 65535
	 * @see {@link #MODE_MINI_HEADER}
	 */
	static protected final int MINI_HEADER_MAX_KNOWN_UNCOMPRESSED_SIZE = BigIntegerUtils.GetMaxValue(MINI_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE * Byte.SIZE, false).intValue();
	
	static protected final int GetUncompressedSizeFieldSize(int mode)
	{
		if(mode == MODE_SPEC_HEADER)
			return SPEC_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE;
		else if(mode == MODE_MINI_HEADER)
			return MINI_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE;
		else /*should never happen*/
			return 0;
	}
	
	/**
	 * @return LZMA options used in all modes (we use the same settings as in {@link LZMA2Compressor}).
	 */
	static private LZMA2Options GetOptions()
	{
		return LZMA2Compressor.GetOptions();
	}
	
	private final int mode;
	
	public LZMACompressor()
	{
		this(DEFAULT_MODE);
	}

	public LZMACompressor(int mode)
	{
		this.mode = mode;
	}

	@Override
	protected LZMAOutputStream _getOutputStream(OutputStream sink, long uncompressedSize) throws IOException
	{
		if(mode == MODE_SPEC_HEADER)
		{
			// Return LZMAOutputStream configured to write spec-compliant .lzma file format stream:
			return new FlushableLZMAOutputStream(sink, GetOptions(), uncompressedSize); // EoPM will only be used if size is unknown
		}
		else
		{
			if(mode == MODE_MINI_HEADER)
			{
				// Check uncompressed size:
				if(uncompressedSize != UNKNOWN_UNCOMPRESSED_SIZE)
				{
					// Manipulate size field range (see {@link #MODE_MINI_HEADER}):
					if(	uncompressedSize < MINI_HEADER_MIN_KNOWN_UNCOMPRESSED_SIZE ||
							uncompressedSize > MINI_HEADER_MAX_KNOWN_UNCOMPRESSED_SIZE)
						uncompressedSize = UNKNOWN_UNCOMPRESSED_SIZE;
					else
						uncompressedSize--;
				}
				// Write MINIh (non-compliant):
				writeSize(sink, uncompressedSize);
			}

			// Decide on EoPM:
			final boolean useEoPM = (mode == MODE_NO_HEADER || uncompressedSize == UNKNOWN_UNCOMPRESSED_SIZE);

			// Return LZMAOutputStream configured to write raw (headerless) LZMA data stream ...
			return new FlushableLZMAOutputStream(
					// ... but get rid of the first meaningless byte (B0):
					new HeaderEatingOutputStream(sink, B0.length),
					GetOptions(),
					useEoPM);
		}
	}

	@Override
	public LZMAInputStream getInputStream(InputStream source) throws IOException
	{
		if(mode == MODE_SPEC_HEADER)
		{
			// Return LZMAInputStream configured to read spec-compliant .lzma file format stream:
			return new LZMAInputStream(source);
		}
		else //if(mode == MODE_MINI_HEADER || mode == MODE_NO_HEADER)
		{
			long uncompressedSize = UNKNOWN_UNCOMPRESSED_SIZE;
			if(mode == MODE_MINI_HEADER)
			{
				// Read MINIh (non-compliant):
				uncompressedSize = readSize(source);
				// Manipulate size field range (see {@link #MODE_MINI_HEADER}):
				if(uncompressedSize != UNKNOWN_UNCOMPRESSED_SIZE)
					uncompressedSize++;
			}

			// Return LZMAInputStream configured to read raw (headerless) LZMA data stream ...
			return new OptionsTakingLZMAInputStream(
					// ... but insert fake B0 byte:
					new SequenceInputStream(new ByteArrayInputStream(B0), source),
					uncompressedSize,
					GetOptions());
		}
	}
	
	/**
	 * Writes the Uncompressed Size field of the header, either as part of the SPECh (compliant
	 * with the LZMA file format specification) or as the only field in the MINIh.
	 * In both cases the given {@code size} value is stored as unsigned little endian integer of
	 * {@value SPEC_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE} bytes (64 bits) for the SPECh or
	 * {@value MINI_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE} bytes (16 bits) for the MINIh.
	 * When the uncompressed size is unknown {@link #UNKNOWN_UNCOMPRESSED_SIZE} is used.
	 * 
	 * @param out
	 * @param size
	 * @throws IOException
	 */
	private void writeSize(OutputStream out, long size) throws IOException
	{
		final int numberOfBytes = GetUncompressedSizeFieldSize(mode);
		for(int i = 0; i < numberOfBytes; i++)
			out.write((int) (size >>> (8 * i)) & 0xFF);
	}
	
	/**
	 * Reads the Uncompressed Size field of the header, either as part of the SPECh (compliant
	 * with the LZMA file format specification) or as the only field in the MINIh.
	 * In both cases the given {@code size} value is stored as unsigned little endian integer of
	 * {@value SPEC_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE} bytes (64 bits) for the SPECh or
	 * {@value MINI_HEADER_UNCOMPRESSED_SIZE_FIELD_SIZE} bytes (16 bits) for the MINIh.
	 * When the uncompressed size is unknown {@link #UNKNOWN_UNCOMPRESSED_SIZE} is used.
	 * 
	 * @param in
	 * @return
	 * @throws IOException
	 */
	private long readSize(InputStream in) throws IOException
	{
		int numberOfBytes = GetUncompressedSizeFieldSize(mode);
		long size = 0;
		BigInteger max = BigIntegerUtils.GetMaxValue(numberOfBytes * Byte.SIZE, false);
		for(int i = 0; i < numberOfBytes; i++)
		{
			int v = in.read();
			if(v < 0)
				throw new IOException("Can't read stream size");
			size |= ((long) v) << (8 * i);
		}
		// Maximum value (all bytes are = 0xFF) represents unknown size (= -1):
		return max.compareTo(BigInteger.valueOf(size)) == 0 ? UNKNOWN_UNCOMPRESSED_SIZE : size;
	}
	
	@Override
	public Compression getMode()
	{
		return Compression.LZMA;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.compression.Compressor#toString()
	 */
	@Override
	public String toString()
	{
		return super.toString() + "_[Mode:" + (mode == MODE_SPEC_HEADER ? "SPECh" : (mode == MODE_MINI_HEADER ? "MINIh" : "NOh")) + "]";
	}

	/**
	 * @author mstevens
	 */
	private class FlushableLZMAOutputStream extends LZMAOutputStream
	{

		public FlushableLZMAOutputStream(OutputStream out, LZMA2Options options, long inputSize) throws IOException
		{
			super(out, options, inputSize);
		}

		public FlushableLZMAOutputStream(OutputStream out, LZMA2Options options, boolean useEndMarker) throws IOException
		{
			super(out, options, useEndMarker);
		}

		@Override
		public void flush()
		{
			// do nothing.
		}

	}

	/**
	 * @author mstevens
	 */
	private class OptionsTakingLZMAInputStream extends LZMAInputStream
	{

		public OptionsTakingLZMAInputStream(InputStream in, long uncompSize, LZMA2Options options) throws IOException, NullPointerException
		{
			super(in, uncompSize, options.getLc(), options.getLp(), options.getPb(), options.getDictSize(), options.getPresetDict());
		}

	}

}
