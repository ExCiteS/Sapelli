package uk.ac.ucl.excites.transmission.compression;

import uk.ac.ucl.excites.transmission.compression.CompressorFactory.CompressionMode;

/**
 * @author mstevens
 *
 */
public class DummyCompressor extends Compressor
{

	@Override
	public byte[] compress(byte[] data) throws CompressorException
	{
		return data;
	}

	@Override
	public byte[] decompress(byte[] compressedData) throws CompressorException
	{
		return compressedData;
	}

	@Override
	public CompressionMode getMode()
	{
		return CompressionMode.NONE;
	}
	
}
