package uk.ac.ucl.excites.util;

import java.util.Arrays;

/**
 * 
 */

/**
 * @author mstevens
 *
 */
public final class BinaryHelpers
{
	
	private BinaryHelpers() { } //should not be instantiated
	
	static public byte[] subByteArray(byte[] array, int offset, int length)
	{
		int to = offset + length;
		if(to > array.length)
			to = array.length;
		return Arrays.copyOfRange(array, offset, to);
	}
	
	static public String toHexadecimealString(byte[] data, int length)
	{
		StringBuffer bff = new StringBuffer();
		//zero padding:
		for(int i = data.length; i < length; i++)
			bff.append("00");
		//the bytes:
		for(byte b : data)
			bff.append(String.format("%02X", b));
		return bff.toString();
	}
	
	static public String toHexadecimealString(byte[] data)
	{
		return toHexadecimealString(data, data.length);
	}
		
	static public String toBinaryString(byte b)
	{
		String str = "";
		for(int i = 7; i >= 0; i--)
			str += ((b & (1 << i)) != 0) ? "1" : "0";
		return str;
	}
	
	/**
	 * Computes the minimum number of bytes needed to fit the given number of bits
	 * 
	 * @param bits
	 * @return number of bytes needed
	 */
	static public int bytesNeeded(int bits)
	{
		return (bits + 7) / 8;
	}

}
