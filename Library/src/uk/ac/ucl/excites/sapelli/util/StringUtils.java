/**
 * 
 */
package uk.ac.ucl.excites.sapelli.util;

import java.nio.charset.Charset;
import java.util.Arrays;
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
	
	static public String addVariableFrontPadding(String str, int desiredLength, char padding)
	{
		if(str == null)
			return str;
		StringBuffer bff = new StringBuffer();
		for(int i = str.length(); i < desiredLength; i++)
			bff.append(padding);
		bff.append(str);
		return bff.toString();
	}
	
	static public String addFixedFrontPadding(String str, int paddingLength, char padding)
	{
		StringBuffer bff = new StringBuffer();
		for(int i = 0; i < paddingLength; i++)
			bff.append(padding);
		bff.append(str);
		return bff.toString();
	}
	
	static public String addTabsFront(String str, int tabs)
	{
		return addFixedFrontPadding(str, tabs, '\t');
	}
	
	static public String join(String[] parts, String separator)
	{
		return join(Arrays.asList(parts), separator);
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
		{
			bff.append(separator);
			bff.append(i.next());
		}
		return bff.toString();
	}
	
	static public String replaceWhitespace(String str, String replacement)
	{
		return str.replaceAll("\\s+", replacement);
	}
	
	/**
	 * Simple escaping method to clear Strings of a single to-be-avoided char
	 * 
	 * @param str
	 * @param avoid
	 * @param replacement
	 * @param prefix
	 * @return
	 */
	static public String escape(String str, char avoid, char replacement, char prefix)
	{
		StringBuilder bldr = new StringBuilder();
		for(char c : str.toCharArray())
			if(c == avoid)
			{	// avoid --> prefix + replacement
				bldr.append(prefix);
				bldr.append(replacement);
			}
			else
			{
				bldr.append(c);
				if(c == prefix)
					bldr.append(prefix); // prefix --> prefix + prefix
			}
		return bldr.toString();
	}
	
	/**
	 * Simple de-escaping method which reverses the work of {@link #escape(String, char, char, char)}
	 * 
	 * @param str
	 * @param avoid
	 * @param replacement
	 * @param prefix
	 * @return
	 */
	static public String deescape(String str, char avoid, char replacement, char prefix)
	{
		StringBuilder bldr = new StringBuilder();
		boolean prevPrefix = false;
		for(char c : str.toCharArray())
		{
			if(prevPrefix)
			{
				if(c == replacement)
					bldr.append(avoid); // prefix + replacement --> avoid
				else
					bldr.append(prefix); // prefix + prefix --> prefix
				prevPrefix = false;
			}
			else
			{
				if(c == prefix)
					prevPrefix = true;
				else
					bldr.append(c);
			}				
		}
		return bldr.toString();
	}
	
	static public int countOccurances(String haystack, char needle)
	{
		int count = 0;
		for(char c : haystack.toCharArray())
			if(c == needle)
				count++;
		return count;
	}
	
}
