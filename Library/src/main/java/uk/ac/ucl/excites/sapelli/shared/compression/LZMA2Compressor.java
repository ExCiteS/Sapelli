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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.tukaani.xz.FinishableWrapperOutputStream;
import org.tukaani.xz.LZMA2Options;
import org.tukaani.xz.UnsupportedOptionsException;

import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory.Compression;

/**
 * LZMA2 compressor.
 * Uses the "XZ for Java" library by Lasse Collin (public domain).
 * 
 * @author mstevens
 * 
 * @see <a href="http://en.wikipedia.org/wiki/LZMA#LZMA2_format">http://en.wikipedia.org/wiki/LZMA#LZMA2_format</a>
 * @see <a href="http://tukaani.org/xz/java.html">http://tukaani.org/xz/java.html</a>
 */
public class LZMA2Compressor extends Compressor
{

	static private LZMA2Options OPTIONS;

	/**
	 * @return options to be used when (de)compressing
	 */
	static /*package*/ LZMA2Options GetOptions()
	{
		if(OPTIONS == null)
			try
			{
				OPTIONS = new LZMA2Options(LZMA2Options.PRESET_MAX);

				// Set options:
				OPTIONS.setDictSize(1 << 20); // Default: 1 << 23 (= 8 << 20); but then it uses way too much memory!
				//OPTIONS.setNiceLen(LZMA2Options.NICE_LEN_MAX); // = "NumFastBytes" in lzma-java
				OPTIONS.setMatchFinder(LZMA2Options.MF_BT4);
			}
			catch (UnsupportedOptionsException ignore) {}
		return OPTIONS;
	}
	
	@Override
	protected OutputStream _getOutputStream(OutputStream sink, long uncompressedSizeBytes)
	{
		return GetOptions().getOutputStream(new FinishableWrapperOutputStream(sink));
	}
	
	@Override
	public InputStream getInputStream(InputStream source) throws IOException
	{
		return GetOptions().getInputStream(source);
	}

	@Override
	public Compression getMode()
	{
		return Compression.LZMA2;
	}

}
