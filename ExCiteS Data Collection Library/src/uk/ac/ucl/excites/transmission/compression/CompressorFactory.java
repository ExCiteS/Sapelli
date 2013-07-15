package uk.ac.ucl.excites.transmission.compression;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Arrays;

/**
 * 
 * @author mstevens
 */
public class CompressorFactory
{
	
	static final DecimalFormat RATIO_FORMAT = new DecimalFormat("##.00");
	
	static public enum CompressionMode
	{
		NONE,
		GZIP,
		LZMA2,
		BZIP2,
		DEFLATE,
		LZMA
		/*HUFFMAN*/
	}
	
	static public Compressor getCompressor(CompressionMode mode)
	{
		switch(mode)
		{
			case NONE		: return new DummyCompressor();
			case DEFLATE	: return new DeflateCompressor();
			case GZIP		: return new GZipCompressor();
			case LZMA		: return new LZMACompressor();
			case LZMA2		: return new LZMA2Compressor();
			case BZIP2		: return new BZIP2Compressor();
			default			: return new DummyCompressor();
		}
	}
	
	static public void CompressionTest(byte[] data)
	{
		CompressionTest(data, CompressorFactory.CompressionMode.values()); // will test all modes
	}
	
	static public void CompressionTest(byte[] data, CompressionMode[] modes)
	{
		CompressionMode best = CompressionMode.NONE;
		int bestSize = data.length;
		try
		{
			for(CompressionMode mode : modes)
			{
				Compressor compressor = CompressorFactory.getCompressor(mode);
				byte[] compressedData = compressor.compress(data);
				System.out.print(" - " + mode + ": " + compressedData.length + " bytes (" + RATIO_FORMAT.format(compressedData.length / (float) data.length * 100f) + "%)");
				if(Arrays.equals(data, compressor.decompress(compressedData)))
				{
					if(compressedData.length < bestSize)
					{
						best = mode;
						bestSize = compressedData.length;
					}
					System.out.println("");
				}
				else
					System.out.println(" -- DECOMPRESSED DATA DOES NOT MATCH INPUT DATA!");
			}
			System.out.println("Best: " + best);
		}
		catch(IOException e)
		{
			e.printStackTrace(System.err);
		}
	}

	static public CompressionResult ApplyBestCompression(byte[] data)
	{
		return ApplyBestCompression(data, CompressorFactory.CompressionMode.values()); // will try all modes
	}
	
	static public CompressionResult ApplyBestCompression(byte[] data, CompressionMode[] modes)
	{
		CompressionResult best = null;		
		try
		{
			for(CompressionMode mode : modes)
			{
				Compressor compressor = CompressorFactory.getCompressor(mode);
				byte[] compressedData = compressor.compress(data);
				if(Arrays.equals(data, compressor.decompress(compressedData)))
				{
					if(best == null || compressedData.length < best.compressedData.length)
						best = new CompressionResult(mode, compressedData, compressedData.length / (float) data.length);
				}
				else
					System.err.println(mode + ": DECOMPRESSED DATA DOES NOT MATCH INPUT DATA!");
			}
		}
		catch(IOException e)
		{
			e.printStackTrace(System.err);
		}
		return best != null ? best : new CompressionResult(CompressionMode.NONE, data, 1.0f);
	}
	
	static public class CompressionResult
	{
		
		private CompressionMode mode;
		private byte[] compressedData;
		private float ratio;
		
		/**
		 * @param mode
		 * @param compressedData
		 * @param ratio
		 */
		public CompressionResult(CompressionMode mode, byte[] compressedData, float ratio)
		{
			this.mode = mode;
			this.compressedData = compressedData;
			this.ratio = ratio;
		}

		/**
		 * @return the mode
		 */
		public CompressionMode getMode()
		{
			return mode;
		}

		/**
		 * @return the compressedData
		 */
		public byte[] getCompressedData()
		{
			return compressedData;
		}

		/**
		 * @return the ratio
		 */
		public float getRatio()
		{
			return ratio;
		}
		
		@Override
		public String toString()
		{
			return mode + "-compressed data is " + compressedData.length + " bytes long (" + RATIO_FORMAT.format(ratio * 100.0f) + " %)"; 
		}
		
	}

}
