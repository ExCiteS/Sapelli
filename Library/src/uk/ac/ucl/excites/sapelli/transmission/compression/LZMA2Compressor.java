package uk.ac.ucl.excites.sapelli.transmission.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;
import org.tukaani.xz.FinishableOutputStream;
import org.tukaani.xz.LZMA2Options;
import org.tukaani.xz.UnsupportedOptionsException;

import uk.ac.ucl.excites.sapelli.transmission.compression.Compressor;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.Compression;

/**
 * LZMA2 compressor.
 * Based on the "XZ for Java" library (public domain).
 * 
 * @author mstevens
 * @see <a href="http://en.wikipedia.org/wiki/LZMA#LZMA2_format">http://en.wikipedia.org/wiki/LZMA#LZMA2_format</a>
 * @see <a href="http://tukaani.org/xz/java.html">http://tukaani.org/xz/java.html</a>
 */
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
	public byte[] compress(byte[] data) throws IOException
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
        	InputStream in = options.getInputStream(new ByteArrayInputStream(compressedData));
            IOUtils.copy(in, out);
            in.close();
        }
        catch(IOException ioe)
        {
        	throw new IOException("Error upon " + getMode() + " decompression", ioe);
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

	@Override
	public Compression getMode()
	{
		return Compression.LZMA2;
	}

}
