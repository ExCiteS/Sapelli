/**
 * 
 */
package uk.ac.ucl.excites.sapelli.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author mstevens
 *
 */
public abstract class XMLParser extends DefaultHandler
{
	
	// Statics-------------------------------------------------------
	static protected final String ENABLED = "enabled";
	static protected final String DISABLED = "disabled";
	
	// Dynamics------------------------------------------------------
	protected List<String> warnings;
	
	public XMLParser()
	{
		this.warnings = new ArrayList<String>();
	}
	
	protected InputStream open(File xmlFile) throws Exception
	{
		if(xmlFile == null || !xmlFile.exists() || xmlFile.length() == 0)
			throw new IllegalArgumentException("Invalid xmlFile (" + (xmlFile == null ? "null" : xmlFile.getAbsolutePath()) + ")!");
		return new FileInputStream(xmlFile);
	}

	public void parse(InputStream input) throws Exception
	{
		if(input == null)
			throw new IllegalArgumentException("Invalid input stream");
		warnings.clear();
		try
		{
			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();
			XMLReader xr = sp.getXMLReader();
			xr.setContentHandler(this);
			xr.parse(new InputSource(input)); // start parsing (this will call startDocument(), startElement(), ...)
		}
		catch(Exception e)
		{
			System.err.println("XML Parsing Exception = " + e);
			// e.printStackTrace(System.err);
			// return null;
			throw e;
		}
		finally
		{
			try
			{
				input.close();
			}
			catch(IOException ioe)
			{
				ioe.printStackTrace();
			}
		}
	}
	
	protected String readRequiredStringAttribute(String qName, Attributes attributes, String attributeName) throws SAXException
	{
		return readRequiredStringAttribute(qName, attributes, attributeName, null);
	}
	
	protected String readRequiredStringAttribute(String qName, Attributes attributes, String attributeName, String reason) throws SAXException
	{
		String value = attributes.getValue(attributeName);
		if(value == null)
			throw new SAXException(attributeName + " is missing, this is a required attribute of " + qName + (reason != null ? " (" + reason + ")" : "") + ".");
		return value;
	}

	protected String readStringAttribute(Attributes attributes, String attributeName, String defaultValue)
	{
		String text = attributes.getValue(attributeName);
		if(text == null || text.isEmpty())
			return defaultValue;
		else
			return text;
	}

	protected boolean readBooleanAttribute(Attributes attributes, String attributeName, boolean defaultValue)
	{
		String text = attributes.getValue(attributeName);
		if(text == null)
			return defaultValue;
		//else
		text = text.trim();
		if(text.isEmpty())
			return defaultValue;
		else if(text.equalsIgnoreCase(Boolean.TRUE.toString()))
			return Boolean.TRUE;
		else if(text.equalsIgnoreCase(Boolean.FALSE.toString()))
			return Boolean.FALSE;
		else if(text.equalsIgnoreCase(ENABLED))
			return Boolean.TRUE;
		else if(text.equalsIgnoreCase(DISABLED))
			return Boolean.FALSE;
		else
			return defaultValue;
	}
	
	protected int readRequiredIntegerAttribute(String qName, Attributes attributes, String attributeName) throws SAXException
	{
		return readRequiredIntegerAttribute(qName, attributes, attributeName, null);
	}
	
	protected int readRequiredIntegerAttribute(String qName, Attributes attributes, String attributeName, String reason) throws SAXException
	{
		String text = attributes.getValue(attributeName);
		if(text == null || text.isEmpty())
			throw new SAXException(attributeName + " is missing, this is a required attribute of " + qName + (reason != null ? " (" + reason + ")" : "") + ".");
		else
			return Integer.parseInt(text.trim());
	}

	protected int readIntegerAttribute(Attributes attributes, String attributeName, int defaultValue)
	{
		String text = attributes.getValue(attributeName);
		if(text == null || text.isEmpty())
			return defaultValue;
		else
			return Integer.parseInt(text.trim());
	}

	protected float readFloatAttribute(Attributes attributes, String attributeName, float defaultValue)
	{
		String text = attributes.getValue(attributeName);
		if(text == null || text.isEmpty())
			return defaultValue;
		else
			return Float.parseFloat(text.trim());
	}
	
	public List<String> getWarnings()
	{
		return warnings;
	}
	
}
