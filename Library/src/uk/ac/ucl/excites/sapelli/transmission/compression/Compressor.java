package uk.ac.ucl.excites.sapelli.transmission.compression;

import java.io.IOException;
import java.util.concurrent.Callable;

/**
 * @author mstevens
 *
 */
public abstract class Compressor
{

	public abstract CompressorFactory.CompressionMode getMode();
	
	public abstract byte[] compress(byte[] data) throws IOException;

	public abstract byte[] decompress(byte[] compressedData) throws IOException;
	
//	public class CompressorCallable implements Callable<CompressorResult>
//	{
//
//		private byte[] uncompressedData;
//		
//		public CompressorCallable(byte[] uncompressedData)
//		{
//			this.uncompressedData = uncompressedData;
//		}
//		
//		@Override
//		public CompressorResult call() throws Exception
//		{
//			return new CompressorResult(getMode(), com, ratio)
//		}
//		
//	}

}
