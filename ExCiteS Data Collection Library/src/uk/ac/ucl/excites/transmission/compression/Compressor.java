package uk.ac.ucl.excites.transmission.compression;

/**
 * @author mstevens
 *
 */
public abstract class Compressor
{

	public abstract String getCompressionType();
	
	public abstract byte[] compress(byte[] data) throws CompressorException;

	public abstract byte[] decompress(byte[] compressedData) throws CompressorException;

}
