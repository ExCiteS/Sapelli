package uk.ac.ucl.excites.transmission.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;
import org.tukaani.xz.FinishableOutputStream;
import org.tukaani.xz.LZMA2Options;
import org.tukaani.xz.UnsupportedOptionsException;

import uk.ac.ucl.excites.transmission.compression.Compressor;

public class LZMA2Compressor extends Compressor
{

	private LZMA2Options options;
	
	public LZMA2Compressor()
	{
		try
		{
			options = new LZMA2Options(LZMA2Options.PRESET_MAX);
		}
		catch(UnsupportedOptionsException e)
		{
			e.printStackTrace();
		}
	}
	
	@Override
	public String getCompressionType()
	{
		return "LZMA2";
	}

	@Override
	public byte[] compress(byte[] data) throws CompressorException
	{
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        try
        {
        	OutputStream out = options.getOutputStream(new WrappedOutputStream(byteArrayOutputStream));
            out.write(data);
            out.close();
        }
        catch(IOException ioe)
        {
        	throw new CompressorException("Error upon " + getCompressionType() + " compression", ioe);
        }
        return byteArrayOutputStream.toByteArray();
	}

	@Override
	public byte[] decompress(byte[] compressedData) throws CompressorException
	{
        ByteArrayOutputStream out = new ByteArrayOutputStream();   
        try
        {
        	InputStream in = options.getInputStream(new ByteArrayInputStream(compressedData));
            IOUtils.copy(in, out);
            in.close();
        }
        catch(IOException ioe)
        {
        	throw new CompressorException("Error upon " + getCompressionType() + " decompression", ioe);
        }
        return out.toByteArray();
	}

	
	public class WrappedOutputStream extends FinishableOutputStream
	{
		
		private OutputStream wrappedStream;
		
		public WrappedOutputStream(OutputStream wrappedStream)
		{
			this.wrappedStream = wrappedStream;
		}

		@Override
		public void write(int b) throws IOException
		{
			wrappedStream.write(b);
		}

	}

}
