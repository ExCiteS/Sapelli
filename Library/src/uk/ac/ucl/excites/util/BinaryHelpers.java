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

	static public String toHexadecimealString(byte[] data, int length, boolean uppercase)
	{
		StringBuffer bff = new StringBuffer();
		//zero padding:
		for(int i = data.length; i < length; i++)
			bff.append("00");
		//the bytes:
		for(byte b : data)
			bff.append(String.format("%02" + (uppercase ? "X" : "x"), b));
		return bff.toString();
	}
	
	static public String toHexadecimealString(byte[] data, int length)
	{
		return toHexadecimealString(data, length, true);
	}
	
	static public String toHexadecimealString(byte[] data, boolean uppercase)
	{
		return toHexadecimealString(data, data.length, uppercase);
	}

	static public String toHexadecimealString(byte[] data)
	{
		return toHexadecimealString(data, true);
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
	
	/**
	 * Splits a long integer in the requested number equally-sized parts, each consisting of the specified number of bits.
	 * Note that if the total number of allowed bits (numberOfParts * bitsPerPart) is insufficient to represent the entire
	 * number data loss will occur at the most significant bits side.
	 * 
	 * In the returned array the more significant parts come first.
	 * 
	 * @param number
	 * @param numberOfParts
	 * @param bitsPerPart
	 * @return
	 */
	public static long[] splitLong(long number, int numberOfParts, int bitsPerPart)
	{
		long[] parts = new long[numberOfParts];
		for(int i = numberOfParts - 1; i >= 0; i--)
		{
			parts[i] = number % (1 << bitsPerPart);
			number >>= bitsPerPart;
		}
		return parts;
	}
	
	/**
	 * Merges the provided long integers into one larger one. Each part will take up the specified number of bits and parts
	 * are to be provided in most-significant-first order.
	 * If any of the parts requires more than the allowed number of bits to be represented data loss will occur due to some
	 * of the most significant bits of the part (not the merged number) being discarded.
	 * 
	 * @param parts
	 * @param bitsPerPart
	 * @return
	 */
	public static long mergeLong(long[] parts, int bitsPerPart)
	{
		long number = 0;
		for(int i = 0; i < parts.length; i++)
			number += (parts[parts.length - 1 - i] % (1 << bitsPerPart + 1)) << (bitsPerPart * i);
		return number;
	}

}
