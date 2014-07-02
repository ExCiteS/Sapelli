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
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;
import org.itadaki.bzip2.BZip2InputStream;
import org.itadaki.bzip2.BZip2OutputStream;

import uk.ac.ucl.excites.sapelli.transmission.compression.Compressor;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.CompressionMode;

/**
 * BZIP2 compressor.
 * Uses the jbzip2 library (MIT licensed).
 * 
 * @author mstevens
 * @see <a href="http://en.wikipedia.org/wiki/Bzip2">http://en.wikipedia.org/wiki/Bzip2</a>
 * @see <a href="http://code.google.com/p/jbzip2">http://code.google.com/p/jbzip2</a>
 */
public class BZIP2Compressor extends Compressor
{
	
	@Override
	public byte[] compress(byte[] data) throws IOException
	{
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        try
        {
        	OutputStream out = new BZip2OutputStream(byteArrayOutputStream);
            out.write(data);
            out.close();
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
        try
        {
        	InputStream in = new BZip2InputStream(new ByteArrayInputStream(compressedData), false);
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
		return CompressionMode.BZIP2;
	}

}
