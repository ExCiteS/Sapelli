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

package uk.ac.ucl.excites.sapelli.shared.util;

import java.util.Arrays;

/**
 * @author mstevens
 * 
 */
public final class BinaryHelpers
{

	private BinaryHelpers()
	{
	} // should not be instantiated

	/**
	 * @param array
	 * @param offset inclusive!
	 * @param length
	 * @return
	 */
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
		// zero padding:
		for(int i = data.length; i < length; i++)
			bff.append("00");
		// the bytes:
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
	 * Returns the number of bits needed to represent the {@code long} value.
	 * Note: always returns 64 for negative values due to 2's complement representation!
	 * 
	 * @param value
	 * @return number of bits needed
	 */
	/*static public int bitsNeeded(long value)
	{
		int count = 0;
		while(value != 0)
		{
			++count;
			value >>>= 1;
		}
		return count;
		//In 1 line: return Long.SIZE - Long.numberOfLeadingZeros(value);
	}*/

	/**
	 * Splits a long integer in the requested number equally-sized parts, each consisting of the specified number of bits. Note that if the total number of
	 * allowed bits (numberOfParts * bitsPerPart) is insufficient to represent the entire number data loss will occur at the most significant bits side.
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
	 * Merges the provided long integers into one larger one. Each part will take up the specified number of bits and parts are to be provided in
	 * most-significant-first order. If any of the parts requires more than the allowed number of bits to be represented data loss will occur due to some of the
	 * most significant bits of the part (not the merged number) being discarded.
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
