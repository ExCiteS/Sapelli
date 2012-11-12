/**
 * 
 */
package uk.ac.ucl.excites.transmission.lib.util;

import java.math.BigInteger;

/**
 * @author mstevens
 *
 */
public final class BinaryHelpers
{
	
	private BinaryHelpers() { } //should not be instantiated
	
	public static String bin2Hex(byte[] data)
	{
		return String.format("%0" + (data.length*2) + "X", new BigInteger(1, data));
	}
	
	public static String toBinaryString(byte b)
	{
		String str = "";
		for(int i = 7; i >= 0; i--)
			str += ((b & (1 << i)) != 0) ? "1" : "0";
		return str;
	}

}
