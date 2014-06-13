package uk.ac.ucl.excites.sapelli.transmission.compression;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * 
 * @author mstevens
 */
public class CompressorFactory
{
	
	static public final DecimalFormat RATIO_FORMAT = new DecimalFormat("##.00");
	
	static public enum Compression
	{
		NONE,
		DEFLATE,
		GZIP,
		LZMA,
		LZMA2,
		BZIP2,
		/*HUFFMAN,*/
	}
	
	static public Compressor getCompressor(Compression mode)
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
		CompressionTest(data, CompressorFactory.Compression.values()); // will test all modes
	}
	
	static public void CompressionTest(byte[] data, Compression[] modes)
	{
		Compression best = Compression.NONE;
		int bestSize = data.length;
		try
		{
			for(Compression mode : modes)
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

	static public CompressorResult ApplyBestCompression(byte[] data)
	{
		return ApplyBestCompression(data, CompressorFactory.Compression.values()); // will try all supported modes
	}
	
	private static final int NUM_CORES = Runtime.getRuntime().availableProcessors();
	
	static public CompressorResult ApplyBestCompression(byte[] data, Compression[] modes)
	{
		//TODO make this multi-threaded?
		//CompressorResult[] results = new CompressorResult[modes.length];
		//ExecutorService executor = Executors.newFixedThreadPool(NUM_CORES);
		
		CompressorResult best = null;
		try
		{
			for(Compression mode : modes)
			{
				Compressor compressor = CompressorFactory.getCompressor(mode);
				byte[] compressedData = compressor.compress(data);
				if(Arrays.equals(data, compressor.decompress(compressedData)))
				{
					if(best == null || compressedData.length < best.getCompressedData().length)
						best = new CompressorResult(mode, compressedData, compressedData.length / (float) data.length);
				}
				else
					System.err.println(mode + ": DECOMPRESSED DATA DOES NOT MATCH INPUT DATA!");
			}
		}
		catch(IOException e)
		{
			e.printStackTrace(System.err);
		}
		return best != null ? best : new CompressorResult(Compression.NONE, data, 1.0f);
	}
	
}
