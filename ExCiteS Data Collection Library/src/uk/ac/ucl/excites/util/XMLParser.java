/**
 * 
 */
package uk.ac.ucl.excites.util;

import java.util.ArrayList;
import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author mstevens
 *
 */
public abstract class XMLParser extends DefaultHandler
{
	
	protected List<String> warnings;

	public XMLParser()
	{
		this.warnings = new ArrayList<String>();
	}
	
	protected String readRequiredStringAttribute(String qName, Attributes attributes, String attributeName) throws SAXException
	{
		String value = attributes.getValue(attributeName);
		if(value == null)
			throw new SAXException(attributeName + " is missing, this is a required attribute of " + qName + ".");
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
		if(text == null || text.isEmpty())
			return defaultValue;
		else if(text.trim().equalsIgnoreCase(Boolean.TRUE.toString()))
			return Boolean.TRUE;
		else if(text.trim().equalsIgnoreCase(Boolean.FALSE.toString()))
			return Boolean.FALSE;
		else
			return defaultValue;
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
