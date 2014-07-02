package uk.ac.ucl.excites.sapelli.transmission.compression;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import lzma.sdk.lzma.Decoder;
import lzma.sdk.lzma.Encoder;
import uk.ac.ucl.excites.sapelli.transmission.compression.Compressor;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.Compression;

/**
 * LZMA compressor.<br/>
 * Based on the lzma-java library by Julien Ponge (jponge) et al. (Apache License 2.0).
 * 
 * We do not use the LZMAOutputStream and LZMAInputStream classes (also provided by the lzma-java library)
 * because they write a header (containing encoder settings and uncompressed data length) which we do not
 * need here, this saves us 18 bytes (5 for the settings, 8 for the length).
 * 
 * @author mstevens
 * @see <a href="http://en.wikipedia.org/wiki/LZMA">http://en.wikipedia.org/wiki/LZMA</a>
 * @see <a href="https://github.com/jponge/lzma-java">https://github.com/jponge/lzma-java</a>
 */
public class LZMACompressor extends Compressor
{

	static public final boolean USE_HEADER = false;
	
	private Encoder encoder;
	private Decoder decoder;
	private boolean useHeader;

	public LZMACompressor()
	{
		this(USE_HEADER);
	}
	
	public LZMACompressor(boolean useHeader)
	{
		this.useHeader = useHeader;
		
		// Encoder:
		this.encoder = new Encoder();

		// Properties:
		encoder.setDictionarySize(1 << 23);
		encoder.setEndMarkerMode(true); // takes up 5 bytes, but if we don't do it decoding fails (even if we pass it the data size instead of -1; see below)
		encoder.setMatchFinder(Encoder.EMatchFinderTypeBT4);
		encoder.setNumFastBytes(0x20);

		// Decoder:
		this.decoder = new Decoder();
		if(!decoder.setDecoderProperties(getProperties()))
			throw new IllegalArgumentException("Incorrect stream properties");
	}

	private byte[] getProperties()
	{
		ByteArrayOutputStream propertiesStream = new ByteArrayOutputStream();
		try
		{
			encoder.writeCoderProperties(propertiesStream);
		}
		catch(IOException ioe)
		{
			ioe.printStackTrace(System.err);
		}
		return propertiesStream.toByteArray();
	}

	@Override
	public byte[] compress(byte[] data) throws IOException
	{
		InputStream in = new BufferedInputStream(new ByteArrayInputStream(data));
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		OutputStream out = new BufferedOutputStream(byteArrayOutputStream);
		try
		{
			if(useHeader)
			{
				// Write header with coder settings (optional, takes up 5 bytes)
				encoder.writeCoderProperties(out);

				// Write header with uncompressed data size (optional, takes up 8 bytes):
				for(int i = 0; i < 8; i++)
					out.write((int) (data.length >>> (8 * i)) & 0xFF);
			}
			
			// Compress & write compressed bytes:
			encoder.code(in, out, -1, -1, null);
			out.flush();
			out.close();
			in.close();
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
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		OutputStream out = new BufferedOutputStream(byteArrayOutputStream);
		InputStream in = new BufferedInputStream(new ByteArrayInputStream(compressedData));
		try
		{
			long outSize = -1;
			if(useHeader)
			{
				// Read header with coder settings (optional):
				byte[] properties = new byte[5];
				if(in.read(properties) != 5)
					throw new IOException("LZMA stream has no header!");
				if(!decoder.setDecoderProperties(properties))
					throw new IOException("Decoder properties cannot be set!");
	
				// Read header with uncompressed data size (optional):
				outSize = 0;
				for(int i = 0; i < 8; i++)
				{
					int v = in.read();
					if(v < 0)
						throw new IOException("Can't read stream size");
					outSize |= ((long) v) << (8 * i);
				}
			}
				
			// Read compressed bytes & decompress:
			if(!decoder.code(in, out, outSize))
				throw new IOException("Error in data stream");
			out.flush();
			out.close();
			in.close();
		}
		catch(IOException ioe)
		{
			throw new IOException("Error upon " + getMode() + " decompression", ioe);
		}
		return byteArrayOutputStream.toByteArray();
	}

	@Override
	public Compression getMode()
	{
		return Compression.LZMA;
	}

}
