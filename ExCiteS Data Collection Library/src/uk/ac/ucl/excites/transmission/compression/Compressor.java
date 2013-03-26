package uk.ac.ucl.excites.transmission.compression;

import java.io.IOException;

/**
 * @author mstevens
 *
 */
public abstract class Compressor
{

	public abstract CompressorFactory.CompressionMode getMode();
	
	public abstract byte[] compress(byte[] data) throws IOException;

	public abstract byte[] decompress(byte[] compressedData) throws IOException;

}
