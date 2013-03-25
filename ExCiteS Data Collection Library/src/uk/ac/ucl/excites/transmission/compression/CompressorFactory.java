package uk.ac.ucl.excites.transmission.compression;

public class CompressorFactory
{
	
	static public enum CompressionMode
	{
		NONE,
		/*HUFFMAN,*/
		GZIP,
		LZMA2
	}
	
	static public Compressor getCompressor(CompressionMode mode)
	{
		switch(mode)
		{
			case NONE	: return new DummyCompressor();
			case GZIP	: return new GZipCompressor();
			case LZMA2	: return new LZMA2Compressor();
			default		: return new DummyCompressor();
		}
	}

}
