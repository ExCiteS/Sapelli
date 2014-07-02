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

package uk.ac.ucl.excites.sapelli.relay.util;

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
	
	static public String toHexadecimealString(byte[] data)
	{
		StringBuffer bff = new StringBuffer();
		for(byte b : data)
			bff.append(String.format("%02X", b));
		return bff.toString();
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
