package uk.ac.ucl.excites.transmission.lib.util;
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
	
	static public String bin2Hex(byte[] data)
	{
		StringBuffer bff = new StringBuffer();
		for(byte b : data)
			bff.append(String.format("%02X ", b));
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
