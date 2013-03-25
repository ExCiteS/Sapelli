package uk.ac.ucl.excites.transmission.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.apache.commons.io.IOUtils;

import uk.ac.ucl.excites.transmission.compression.CompressorFactory.CompressionMode;

public class GZipCompressor extends Compressor
{

	@Override
	public byte[] compress(byte[] data) throws CompressorException
	{
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        try
        {
            GZIPOutputStream gzipOutputStream = new GZIPOutputStream(byteArrayOutputStream);
            gzipOutputStream.write(data);
            gzipOutputStream.close();
        }
        catch(IOException ioe)
        {
            throw new CompressorException("Error upon " + getMode() + " compression", ioe);
        }
        return byteArrayOutputStream.toByteArray();
	}

	@Override
	public byte[] decompress(byte[] compressedData) throws CompressorException
	{
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try
        {
        	InputStream in = new GZIPInputStream(new ByteArrayInputStream(compressedData));
            IOUtils.copy(in, out);
            in.close();
        }
        catch(IOException ioe)
        {
        	throw new CompressorException("Error upon " + getMode() + " decompression", ioe);
        }
        return out.toByteArray();
	}

	@Override
	public CompressionMode getMode()
	{
		return CompressionMode.GZIP;
	}


}
