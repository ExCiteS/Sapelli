/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.shared.compression;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.DecimalFormat;
import java.util.Arrays;

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
	
	/**
	 * @param mode
	 * @return
	 */
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
	
	/**
	 * @param mode
	 * @param sink
	 * @return
	 * @throws IOException 
	 */
	static public OutputStream getCompressorOutputStream(Compression mode, OutputStream sink) throws IOException
	{
		return getCompressor(mode).getOutputStream(sink);
	}
	
	/**
	 * @param mode
	 * @param source
	 * @return
	 * @throws IOException
	 */
	static public InputStream getCompressorInputStream(Compression mode, InputStream source) throws IOException
	{
		return getCompressor(mode).getInputStream(source);
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

	/**
	 * @param data
	 * @param verify
	 * @return
	 */
	static public CompressorResult ApplyBestCompression(byte[] data, boolean verify)
	{
		return ApplyBestCompression(data, CompressorFactory.Compression.values(), verify); // will try all supported modes
	}
	
	/**
	 * @param data
	 * @param modes
	 * @param verify
	 * @return
	 */
	static public CompressorResult ApplyBestCompression(byte[] data, Compression[] modes, boolean verify)
	{
		//TODO make this multi-threaded?
		//CompressorResult[] results = new CompressorResult[modes.length];
		//ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
		
		CompressorResult best = null;
		for(Compression mode : modes)
		{
			try
			{
				Compressor compressor = CompressorFactory.getCompressor(mode);
				byte[] compressedData = compressor.compress(data);
				if(!verify || Arrays.equals(data, compressor.decompress(compressedData)))
				{
					if(best == null || compressedData.length < best.getCompressedData().length)
						best = new CompressorResult(mode, compressedData, compressedData.length / (float) data.length);
				}
				else
					System.err.println(mode + ": DECOMPRESSED DATA DOES NOT MATCH INPUT DATA!");
			}
			catch(IOException e)
			{
				e.printStackTrace(System.err);
				continue;
			}
		}
		return best != null ? best : new CompressorResult(Compression.NONE, data, 1.0f);
	}
	
}
