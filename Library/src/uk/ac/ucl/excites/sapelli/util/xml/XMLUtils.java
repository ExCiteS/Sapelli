
package uk.ac.ucl.excites.sapelli.util.xml;

import org.apache.xerces.util.XML11Char;
import org.apache.xerces.util.XMLChar;

import uk.ac.ucl.excites.sapelli.util.StringUtils;


/**
 * A class with helpful methods for dealing with XML (some code borrowed from NoiseTube Mobile; Licensed under LGPL v2.1)
 * 
 * @author mstevens
 * 
 */
public class XMLUtils
{
	
	static public String header(String encoding, boolean v11)
	{
		return "<?xml version=\"1." + (v11 ? "1" : "0") + "\" encoding=\"" + encoding + "\"?>";
	}
	
	static public String header()
	{
		return header("UTF-8", false);
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
		return comment(text, 0);
	}
	
	/**
	 * Returns an XML comment string with the given text and the given number of
	 * tabs in front
	 * 
	 * @param text
	 * @param tabs  number of tabs to insert before comment start
	 * @return xml comment String
	 */
	static public String comment(String text, int tabs)
	{
		return StringUtils.addTabsFront("<!-- " + escapeCharacters(text) + " -->", tabs);
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
		if(input == null)
			return input;
		input.replace("&", "&amp;");
		input.replace("<", "&lt;");
		input.replace(">", "&gt;");
		input.replace("\"", "&quot;");
		input.replace("'", "&apos;");
		return input;
	}
	
	/**
	 * Uses {@link XMLChar} & {@link XML11Char}, taken from Apache Xerces2 Java Parser (currently included code files are taken from v2.11.0),
	 * which is licensed under Apache License, Version 2.0.
	 * 
	 * TODO we are including the whole Xerces2-j library just for this one method, possibly proguard clears out unused classes but we need to double check this.
	 * 
	 * @param tagName
	 * @param v11
	 * @return
	 * 
	 * @see <a href="https://xerces.apache.org/xerces2-j/javadocs/xerces2/org/apache/xerces/util/XMLChar.html#isValidName(java.lang.String)">org.apache.xerces.util.XMLChar#isValidName(java.lang.String)</a>
	 * @see <a href="https://xerces.apache.org/xerces2-j/javadocs/xerces2/org/apache/xerces/util/XML11Char.html#isValidName(java.lang.String)">org.apache.xerces.util.XML11Char#isXML11ValidName(java.lang.String)</a>
	 * 
	 */
	static public boolean isValidName(String tagName, boolean v11)
	{
		return v11 ? XML11Char.isXML11ValidName(tagName) : XMLChar.isValidName(tagName);
	}
	
}
