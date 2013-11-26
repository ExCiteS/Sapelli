/**
 *
 */
package uk.ac.ucl.excites.util;

import java.io.File;
import java.io.InputStream;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import uk.ac.ucl.excites.util.XMLParser;

/**
 * Produces a hash code from an XML file or inputstream, it ignores white space and comments
 * 
 * @author mstevens
 * 
 */
public class XMLHasher extends XMLParser
{
	
	static private final int MULTIPLIER = 31;

	private int hashCode;

	public int getHashCode(File xmlFile) throws Exception
	{
		return getHashCode(open(xmlFile), false);
	}

	public int getHashCode(InputStream input, boolean leaveOpen) throws Exception
	{
		hashCode = 1;
		parse(input, leaveOpen);
		return hashCode;
	}
	
	private void update(int code)
	{
		hashCode = MULTIPLIER * hashCode + code;
	}
	
	private void update(Object obj)
	{
		update(obj != null ? obj.hashCode() : 0);
	}
	
	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		//Tag
		update(uri);
		update(localName);
		update(qName);
		
		//Attributes
		for(int i = 0; i < attributes.getLength(); i++)
		{
			update(attributes.getURI(i));
			update(attributes.getLocalName(i));
			update(attributes.getQName(i));
			update(attributes.getType(i));
			update(attributes.getValue(i));
		}
	}
	
	@Override
	public void characters(char ch[], int start, int length) throws SAXException
	{
		update(new String(ch, start, length));
	}

}
