/**
 *
 */
package uk.ac.ucl.excites.util;

import java.io.File;
import java.io.InputStream;
import java.util.zip.CRC32;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import uk.ac.ucl.excites.util.XMLParser;

/**
 * Computes hash codes from an XML file or inputstream, ignoring white space and comments
 * 
 * @author mstevens
 * 
 */
public class XMLHasher extends XMLParser
{
	
	static private final int MULTIPLIER = 31;

	private int hashCode = 1;
	private CRC32 crc = null;
	
	/**
	 * Returns a signed 32bit hash code, computed in "Java-style" (i.e. similarly to {@link Object#hashCode()})
	 * 
	 * @param xmlFile
	 * @return
	 * @throws Exception
	 */
	public int getJavaHashCode(File xmlFile) throws Exception
	{
		return getJavaHashCode(open(xmlFile), false);
	}

	/**
	 * Returns a signed 32bit hash code, computed in "Java-style" (i.e. similarly to {@link Object#hashCode()})
	 * 
	 * @param input
	 * @param leaveOpen
	 * @return
	 * @throws Exception
	 */
	public int getJavaHashCode(InputStream input, boolean leaveOpen) throws Exception
	{
		try
		{
			parse(input, leaveOpen);
			return hashCode;
		}
		finally
		{	//Reset:
			hashCode = 1;
		}
	}
	
	/**
	 * Returns an unsigned 32bit hash code, computed using CRC32
	 * 
	 * @param xmlFile
	 * @return
	 * @throws Exception
	 */
	public long getCRC32HashCode(File xmlFile) throws Exception
	{
		return getCRC32HashCode(open(xmlFile), false);
	}

	/**
	 * Returns an unsigned 32bit hash code, computed using CRC32
	 * 
	 * @param input
	 * @param leaveOpen
	 * @return
	 * @throws Exception
	 */
	public long getCRC32HashCode(InputStream input, boolean leaveOpen) throws Exception
	{
		try
		{
			crc = new CRC32();
			parse(input, leaveOpen);
			return crc.getValue();
		}
		finally
		{	//Reset:
			crc = null;
		}
	}
	
	private void update(String str)
	{
		if(str != null)
		{
			if(crc == null)
				hashCode = MULTIPLIER * hashCode + str.hashCode();
			else
				crc.update(str.getBytes());
		}
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
	
	/*private void update(int code)
	{
		hashCode = MULTIPLIER * hashCode + code;
	}
	
	private void update(Object obj)
	{
		update(obj != null ? obj.hashCode() : 0);
	}*/

}
