/**
 * 
 */
package uk.ac.ucl.excites.transmission.lib.util;

import java.math.BigInteger;

/**
 * @author mstevens
 *
 */
public class BinaryHelpers
{
	
	private BinaryHelpers() { } //should not be instantiated
	
	public static String bin2Hex(byte[] data)
	{
		return String.format("%0" + (data.length*2) + "X", new BigInteger(1, data));
	}

}
