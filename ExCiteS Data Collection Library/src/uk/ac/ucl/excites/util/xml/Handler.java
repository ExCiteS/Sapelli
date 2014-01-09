/**
 * 
 */
package uk.ac.ucl.excites.util.xml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author mstevens
 *
 */
public abstract class Handler extends DefaultHandler
{

	// Static
	protected static final String ENABLED = "enabled";
	protected static final String DISABLED = "disabled";
	
	// Dynamic
	private final Map<String, SubtreeParser> subtreeParsers;
	private SubtreeParser activeSubtreeParser;
	
	protected final List<String> warnings;

	public Handler()
	{
		this.warnings = new ArrayList<String>();
		this.subtreeParsers = new HashMap<String, SubtreeParser>();
		this.activeSubtreeParser = null;
	}
	
	public void addSubtreeParser(SubtreeParser subtreeParser)
	{
		if(subtreeParser != null)
			subtreeParsers.put(subtreeParser.getRootElementQName(), subtreeParser);
		else
			throw new NullPointerException("SubtreeParser cannot be null!");
	}
	
	public void removeSubtreeParser(String rootQName)
	{
		subtreeParsers.remove(rootQName);
	}
	
	public void activateSubtreeParser(SubtreeParser subtreeParser)
	{
		if(subtreeParsers.get(subtreeParser.getRootElementQName()) == null)
			throw new IllegalArgumentException("Unknown SubtreeParser");
		this.activeSubtreeParser = subtreeParser;
	}
	
	public void deactivateSubtreeParser(SubtreeParser subtreeParser)
	{
		if(activeSubtreeParser != subtreeParser)
			return;
		// Copy & clear warnings:
		addWarnings(activeSubtreeParser.getWarnings());
		activeSubtreeParser.clearWarnings();
		// Deactivate:
		this.activeSubtreeParser = null;
		// Remove if needed:
		if(subtreeParser.isSingleUse())
			subtreeParsers.remove(subtreeParser);
	}
	
	/**
	 * @return the activeSubtreeParser
	 */
	public SubtreeParser getActiveSubtreeParser()
	{
		return activeSubtreeParser;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		// Try delegating to active SubtreeParser:
		if(activeSubtreeParser != null)
			activeSubtreeParser.parseStartElement(uri, localName, qName, attributes);
		// Try delegating to subtree parser with matching root element:
		else
		{
			SubtreeParser subtreeParser = subtreeParsers.get(qName);
			if(subtreeParser != null)
			{
				subtreeParser.parseStartElement(uri, localName, qName, attributes); //delegate to subtree parser with mathing root element, it will activate itself if successful
				return; //!!!
			}
		}
		// Handle locally:
		this.parseStartElement(uri, localName, qName, attributes);
	}
	
	/**
	 * To be overridden if necessary
	 * 
	 * @param uri
	 * @param localName
	 * @param qName
	 * @param attributes
	 * @throws SAXException
	 */
	protected void parseStartElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		super.startElement(uri, localName, qName, attributes);
	}
	
	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException
	{
		// Try delegating to active SubtreeParser:
		if(activeSubtreeParser != null)
			activeSubtreeParser.parseEndElement(uri, localName, qName);
		// Handle locally:
		else
			this.parseEndElement(uri, localName, qName);
	}
	
	/**
	 * To be overridden if necessary
	 * 
	 * @param uri
	 * @param localName
	 * @param qName
	 * @throws SAXException
	 */
	protected void parseEndElement(String uri, String localName, String qName) throws SAXException
	{
		super.endElement(uri, localName, qName);
	}

	@Override
	public void characters(char ch[], int start, int length) throws SAXException
	{
		// Try delegating to active SubtreeParser:
		if(activeSubtreeParser != null)
			activeSubtreeParser.parseCharacters(ch, start, length);
		// Handle locally:
		else
			this.parseCharacters(ch, start, length);
	}
	
	/**
	 * To be overridden if necessary
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	protected void parseCharacters(char ch[], int start, int length) throws SAXException
	{
		super.characters(ch, start, length);
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
		//else:
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

	protected void addWarning(String warning)
	{
		warnings.add(warning);
	}

	protected void addWarnings(Collection<String> warnings)
	{
		this.warnings.addAll(warnings);
	}

	public List<String> getWarnings()
	{
		return warnings;
	}
	
	public void clearWarnings()
	{
		warnings.clear();
	}

}
