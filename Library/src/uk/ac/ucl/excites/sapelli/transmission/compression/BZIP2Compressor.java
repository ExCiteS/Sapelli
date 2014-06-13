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
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.Compression;

/**
 * BZIP2 compressor.
 * Based on the jbzip2 library (MIT licensed).
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
	public Compression getMode()
	{
		return Compression.BZIP2;
	}

}
