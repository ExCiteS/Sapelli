package uk.ac.ucl.excites.transmission.compression;

public class CompressorFactory
{
	
	static public enum CompressionMode
	{
		NONE,
		/*HUFFMAN,*/
		GZIP,
		LZMA2,
		BZIP2,
		DEFLATE
	}
	
	static public Compressor getCompressor(CompressionMode mode)
	{
		switch(mode)
		{
			case NONE	: return new DummyCompressor();
			case GZIP	: return new GZipCompressor();
			case LZMA2	: return new LZMA2Compressor();
			case BZIP2	: return new BZIP2Compressor();
			case DEFLATE : return new DeflateCompressor();
			default		: return new DummyCompressor();
		}
	}

}
