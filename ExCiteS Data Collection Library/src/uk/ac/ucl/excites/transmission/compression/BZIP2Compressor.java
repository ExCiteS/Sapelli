package uk.ac.ucl.excites.transmission.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;
import org.itadaki.bzip2.BZip2InputStream;
import org.itadaki.bzip2.BZip2OutputStream;

import uk.ac.ucl.excites.transmission.compression.Compressor;
import uk.ac.ucl.excites.transmission.compression.CompressorFactory.CompressionMode;

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
        	InputStream in = new BZip2InputStream (new ByteArrayInputStream(compressedData), false);
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
