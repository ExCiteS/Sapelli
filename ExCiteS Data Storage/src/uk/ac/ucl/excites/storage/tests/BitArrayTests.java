package uk.ac.ucl.excites.storage.tests;

public class BitArrayTests
{

	private boolean[] bits;
	
	public BitArrayTests(int size)
	{
		bits = new boolean[size];
	}

	public long toUnsignedInt(int start, int size)
	{
		long result = 0;
		for(int b = start; b <= start+size && b <= bits.length; b++)
	      result = result | (bits[b] ? 1 : 0) << b;
		return result;
	}
	
	public boolean[] toSignedBits()
	{
		
		return null;
	}
	
}
