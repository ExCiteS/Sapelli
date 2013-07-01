package uk.ac.ucl.excites.transmission.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.Deflater;
import java.util.zip.DeflaterInputStream;
import java.util.zip.DeflaterOutputStream;

import org.apache.commons.io.IOUtils;

import uk.ac.ucl.excites.transmission.compression.CompressorFactory.CompressionMode;

public class DeflateCompressor extends Compressor
{
	
	private Deflater deflater;
	
	public DeflateCompressor()
	{
		this.deflater = new Deflater(Deflater.BEST_COMPRESSION, true); // best compression & no header
	}

	@Override
	public byte[] compress(byte[] data) throws IOException
	{
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
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
        try
        {
        	InputStream in = new DeflaterInputStream(new ByteArrayInputStream(compressedData), deflater);
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
