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

package uk.ac.ucl.excites.sapelli.transmission.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.Deflater;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;
import java.util.zip.DeflaterOutputStream;

import org.apache.commons.io.IOUtils;

import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.CompressionMode;

/**
 * DEFLATE compressor.
 * Uses Java SE's implementation (java.util.zip).
 * 
 * @author mstevens
 * @see <a href="http://en.wikipedia.org/wiki/DEFLATE">http://en.wikipedia.org/wiki/DEFLATE</a>
 * @see <a href="http://docs.oracle.com/javase/6/docs/api/java/util/zip/package-summary.html">http://docs.oracle.com/javase/6/docs/api/java/util/zip/package-summary.html</a>
 */
public class DeflateCompressor extends Compressor
{
	
	static private final boolean NO_HEADER = true;

	@Override
	public byte[] compress(byte[] data) throws IOException
	{
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		Deflater deflater = new Deflater(Deflater.BEST_COMPRESSION, NO_HEADER); // best compression & no header
        try
        {
        	DeflaterOutputStream deflateOutputStream = new DeflaterOutputStream(byteArrayOutputStream, deflater);
        	deflateOutputStream.write(data);
        	deflateOutputStream.close();
        }
        catch(IOException ioe)
        {
            throw new IOException("Error upon " + getMode() + " compression", ioe);
        }
        return byteArrayOutputStream.toByteArray();
	}

	@Override
	public byte[] decompress(byte[] compressedData) throws IOException
	{
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        Inflater inflater = new Inflater(NO_HEADER);
        try
        {
        	InputStream in = new InflaterInputStream(new ByteArrayInputStream(compressedData), inflater);
            IOUtils.copy(in, out);
            in.close();
        }
        catch(IOException ioe)
        {
        	throw new IOException("Error upon " + getMode() + " decompression", ioe);
        }
        return out.toByteArray();
	}

	@Override
	public CompressionMode getMode()
	{
		return CompressionMode.DEFLATE;
	}

}
