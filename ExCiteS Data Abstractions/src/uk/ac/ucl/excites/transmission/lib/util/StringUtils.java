/**
 * 
 */
package uk.ac.ucl.excites.transmission.lib.util;

import java.nio.charset.Charset;

/**
 * @author mstevens
 *
 */
public final class StringUtils
{

	static private Charset UTF8 = Charset.forName("UTF-8"); 
	
	private StringUtils() {}
	
	/**
	 * Returns the number bytes a given string takes up when encoded with a given charset 
	 * 
	 * @param string
	 * @return number of bytes that would be used to write the string
	 */
	static public int sizeBytes(String string, Charset charset)
	{
		return string.getBytes(charset).length;
	}
	
	/**
	 * Returns the number bytes a given string takes up when encoded as UTF-8 
	 * 
	 * @param string
	 * @return number of bytes that would be used to write the string
	 */
	static public int sizeBytes(String string)
	{
		return string.getBytes(UTF8).length;
	}
	
}
