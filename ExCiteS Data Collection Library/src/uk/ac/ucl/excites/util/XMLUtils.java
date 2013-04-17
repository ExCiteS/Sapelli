
package uk.ac.ucl.excites.util;


/**
 * A class with helpful methods for dealing with XML (some code borrowed from NoiseTube Mobile; Licensed under LGPL v2.1)
 * 
 * @author mstevens
 * 
 */
public class XMLUtils
{

	static public String header(String encoding)
	{
		return "<?xml version=\"1.0\" encoding=\"" + encoding + "\"?>";
	}
	
	static public String header()
	{
		return header("UTF-8");
	}
	
	/**
	 * Returns an XML comment string with the given text and the given number of
	 * tabs in front
	 * 
	 * @param text
	 * @return xml comment String
	 */
	static public String comment(String text)
	{
		return "<!-- " + escapeCharacters(text) + " -->";
	}

	/**
	 * Replaces reserved XML characters with escapes
	 * 
	 * @param input
	 *            a String to process
	 * @return the same String but with reserved XML characters escaped
	 */
	static public String escapeCharacters(String input)
	{
		input.replace("&", "&amp;");
		input.replace("<", "&lt;");
		input.replace(">", "&gt;");
		input.replace("\"", "&quot;");
		input.replace("'", "&apos;");
		return input;
	}
	
}
