/**
 * 
 */
package uk.ac.ucl.excites.util;

import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.List;

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
	
	static public String addTabsFront(String string, int tabs)
	{
		StringBuffer bff = new StringBuffer();
		for(int t = 0; t < tabs; t++)
			bff.append('\t');
		bff.append(string);
		return bff.toString();
	}
	
	static public String join(List<String> parts, String separator)
	{
		if(parts == null)
			return null;
		if(parts.isEmpty())
			return "";
		Iterator<String> i = parts.iterator();
		StringBuffer bff = new StringBuffer(i.next());
		while(i.hasNext())
			bff.append(separator + i.next());
		return bff.toString();
	}
	
}
