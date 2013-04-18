/**
 * 
 */
package uk.ac.ucl.excites.util;

/**
 * ROT13 and "NumROT5" coder
 * 
 * @author mstevens
 * 
 */
public final class ROT13
{

	private ROT13() {}

	public static String rot13(String text)
	{
		StringBuilder bld = new StringBuilder(text.length());
		for(char c : text.toCharArray())
			bld.append(rot13(c));
		return bld.toString();
	}
	
	public static String rot13NumRot5(String text)
	{
		StringBuilder bld = new StringBuilder(text.length());
		for(char c : text.toCharArray())
			bld.append(numRot5(rot13(c)));
		return bld.toString();
	}
	
	private static char rot13(char c)
	{
		if((c >= 'A') && (c <= 'Z'))
			c = (char) ((((c - 'A') + 13) % 26) + 'A');
		if((c >= 'a') && (c <= 'z'))
			c = (char) ((((c - 'a') + 13) % 26) + 'a');
		return c;
	}
	
	private static char numRot5(char c)
	{
		if((c >= '0') && (c <= '9'))
			c = (char) ((((c - '0') + 5) % 10) + '0');
		return c;
	}

}
