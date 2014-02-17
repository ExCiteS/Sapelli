/**
 * 
 */
package uk.ac.ucl.excites.sapelli.util.xml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import uk.ac.ucl.excites.sapelli.util.StringUtils;

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
	
	public void clearSubtreeParsers()
	{
		subtreeParsers.clear();
		activeSubtreeParser = null;
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
		else if(subtreeParsers.containsKey(qName))
			subtreeParsers.get(qName).parseStartElement(uri, localName, qName, attributes); //delegate to subtree parser with matching root element, it will activate itself if successful
		// Handle locally:
		else
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

	/**
	 * Read a required String attribute with name {@code attributeName} in a tag with {@code qName}, using the passed {@code attributes} collection.
	 * 
	 * @param qName
	 * @param attributeName
	 * @param attributes
	 * @param trim
	 * @param allowEmpty
	 * @return
	 * @throws SAXException	when no matching attribute is found
	 */
	protected String readRequiredStringAttribute(String qName, String attributeName, Attributes attributes, boolean trim, boolean allowEmpty) throws SAXException
	{
		return readRequiredStringAttribute(qName, attributeName, null, attributes, trim, allowEmpty);
	}

	/**
	 * Read a required String attribute with a name from {@code attributeNames} (tried in order, first existing attribute wins) in a tag with {@code qName}, using the passed {@code attributes} collection.
	 * 
	 * @param qName
	 * @param attributes
	 * @param trim
	 * @param allowEmpty
	 * @param attributeNames	alternative attribute names ("synonyms")
	 * @return
	 * @throws SAXException	when no matching attribute is found
	 */
	protected String readRequiredStringAttribute(String qName, Attributes attributes, boolean trim, boolean allowEmpty, String... attributeNames) throws SAXException
	{
		return readRequiredStringAttribute(qName, null, attributes, trim, allowEmpty, attributeNames);
	}
	
	/**
	 * Read a required String attribute with name {@code attributeName} in a tag with {@code qName}, using the passed {@code attributes} collection.
	 * The {@code reason} explains why the attribute is required.
	 * 
	 * @param qName
	 * @param attributeName
	 * @param reason
	 * @param attributes
	 * @param trim
	 * @param allowEmpty
	 * @return
	 * @throws SAXException	when no matching attribute is found
	 */
	protected String readRequiredStringAttribute(String qName, String attributeName, String reason, Attributes attributes, boolean trim, boolean allowEmpty) throws SAXException
	{
		return readRequiredStringAttribute(qName, reason, attributes, trim, allowEmpty, attributeName);
	}
	
	/**
	 * Read a required String attribute with a name from {@code attributeNames} (tried in order, first existing attribute wins) in a tag with {@code qName}, using the passed {@code attributes} collection.
	 * The {@code reason} explains why the attribute is required.
	 * 
	 * @param qName
	 * @param reason
	 * @param attributes
	 * @param trim
	 * @param allowEmpty
	 * @param attributeNames	alternative attribute names ("synonyms")
	 * @return
	 * @throws SAXException	when no matching attribute is found
	 */
	protected String readRequiredStringAttribute(String qName, String reason, Attributes attributes, boolean trim, boolean allowEmpty, String... attributeNames) throws SAXException
	{
		for(String attributeName : attributeNames)
		{
			String value = attributes.getValue(attributeName);
			if(value != null)
			{
				if(allowEmpty || !"".equals(trim ? value.trim() : value))
					return trim ? value.trim() : value;
				else
					throw new SAXException("Required attribute " + attributeName + " on tag <" + qName + "> is present but has an empty value."); // don't try next alternative because attribute was present
			}
			//else :  there is no attribute with the attributeName, try next alternative
		}
		throw new SAXException("There is no attribute with name " + StringUtils.join(attributeNames, " or ") + ", this is required for tag <" + qName + ">" + (reason != null ? " (" + reason + ")" : "") + ".");
	}
	
	/**
	 * Read an optional String attribute with name {@code attributeName}, using the passed {@code attributes} collection.
	 * When no such attribute exists the {@code defaultValue} is returned.
	 * 
	 * @param attributeName
	 * @param defaultValue
	 * @param attributes
	 * @param trim
	 * @param allowEmpty
	 * @return
	 */
	protected String readStringAttribute(String attributeName, String defaultValue, Attributes attributes, boolean trim, boolean allowEmpty)
	{
		return readStringAttribute(defaultValue, attributes, trim, allowEmpty, attributeName);
	}
	
	/**
	 * Read an optional String attribute with name from {@code attributeNames} (tried in order, first existing attribute wins), using the passed {@code attributes} collection.
	 * When no such attribute exists the {@code defaultValue} is returned.
	 * 
	 * @param defaultValue
	 * @param attributes
	 * @param trim
	 * @param allowEmpty
	 * @param attributeNames	alternative attribute names ("synonyms")
	 * @return
	 */
	protected String readStringAttribute(String defaultValue, Attributes attributes, boolean trim, boolean allowEmpty, String... attributeNames)
	{
		for(String attributeName : attributeNames)
		{
			String value = attributes.getValue(attributeName);
			if(value != null)
			{
				if(allowEmpty || !"".equals(trim ? value.trim() : value))
					return trim ? value.trim() : value;
				else
					return defaultValue; // attribute is present but empty -> don't try next alternative and return defaultValue
			}
			//else :  there is no attribute with the attributeName, try next alternative
		}
		return defaultValue;
	}

	/**
	 * Read an optional boolean attribute with name {@code attributeName}, using the passed {@code attributes} collection.
	 * When no such attribute exists the {@code defaultValue} is returned.
	 * 
	 * @param attributeName
	 * @param defaultValue
	 * @param attributes
	 * @return
	 */
	protected boolean readBooleanAttribute(String attributeName, boolean defaultValue, Attributes attributes)
	{
		return readBooleanAttribute(defaultValue, attributes, attributeName);
	}
	
	/**
	 * Read an optional boolean attribute with a name from {@code attributeNames} (tried in order, first existing attribute wins), using the passed {@code attributes} collection.
	 * When no such attribute exists the {@code defaultValue} is returned.
	 * 
	 * @param defaultValue
	 * @param attributes
	 * @param attributeNames	alternative attribute names ("synonyms")
	 * @return
	 */
	protected boolean readBooleanAttribute(boolean defaultValue, Attributes attributes, String... attributeNames)
	{
		for(String attributeName : attributeNames)
		{
			String strVal = attributes.getValue(attributeName);
			if(strVal == null)
				continue; // there is no attribute with the attributeName, try next alternative
			else
			{
				strVal = strVal.trim();
				if(strVal.isEmpty())
					return defaultValue;
				else if(strVal.equalsIgnoreCase(Boolean.TRUE.toString()))
					return Boolean.TRUE;
				else if(strVal.equalsIgnoreCase(Boolean.FALSE.toString()))
					return Boolean.FALSE;
				else if(strVal.equalsIgnoreCase(ENABLED))
					return Boolean.TRUE;
				else if(strVal.equalsIgnoreCase(DISABLED))
					return Boolean.FALSE;
				else
					return defaultValue;				
			}
		}
		return defaultValue;
	}

	/**
	 * Read a required integer attribute with name {@code attributeName} in a tag with {@code qName}, using the passed {@code attributes} collection.
	 * 
	 * @param qName
	 * @param attributeName
	 * @param attributes
	 * @return
	 * @throws SAXException	when no matching attribute is found
	 * @throws NumberFormatException when the attribute value string does not hold a valid integer (e.g. because it is empty)
	 */
	protected int readRequiredIntegerAttribute(String qName, String attributeName, Attributes attributes) throws SAXException, NumberFormatException
	{
		return readRequiredIntegerAttribute(qName, attributeName, null, attributes);
	}
	
	/**
	 * Read a required integer attribute with a name from {@code attributeNames} (tried in order, first existing attribute wins) in a tag with {@code qName}, using the passed {@code attributes} collection.
	 * 
	 * @param qName
	 * @param attributes
	 * @param attributeNames	alternative attribute names ("synonyms")
	 * @return
	 * @throws SAXException	when no matching attribute is found
	 * @throws NumberFormatException when the attribute value string does not hold a valid integer (e.g. because it is empty)
	 */
	protected int readRequiredIntegerAttribute(String qName, Attributes attributes, String... attributeNames) throws SAXException, NumberFormatException
	{
		return readRequiredIntegerAttribute(qName, null, attributes, attributeNames);
	}
	
	/**
	 * Read a required integer attribute with name {@code attributeName} in a tag with {@code qName}, using the passed {@code attributes} collection.
	 * The {@code reason} explains why the attribute is required.
	 * 
	 * @param qName
	 * @param attributeName
	 * @param reason
	 * @param attributes
	 * @return
	 * @throws SAXException	when no matching attribute is found
	 * @throws NumberFormatException when the attribute value string does not hold a valid integer (e.g. because it is empty)
	 */
	protected int readRequiredIntegerAttribute(String qName, String attributeName, String reason, Attributes attributes) throws SAXException, NumberFormatException
	{
		return readRequiredIntegerAttribute(qName, reason, attributes, attributeName);
	}
	
	/**
	 * Read a required integer attribute with a name from {@code attributeNames} (tried in order, first existing attribute wins) in a tag with {@code qName}, using the passed {@code attributes} collection.
	 * The {@code reason} explains why the attribute is required.
	 * 
	 * @param qName
	 * @param reason
	 * @param attributes
	 * @param attributeNames	alternative attribute names ("synonyms")
	 * @return
	 * @throws SAXException	when no matching attribute is found
	 * @throws NumberFormatException when the attribute value string does not hold a valid integer (e.g. because it is empty)
	 */
	protected int readRequiredIntegerAttribute(String qName, String reason, Attributes attributes, String... attributeNames) throws SAXException, NumberFormatException
	{
		for(String attributeName : attributeNames)
		{
			String strVal = attributes.getValue(attributeName);
			if(strVal != null)
				return Integer.parseInt(strVal.trim()); // throws NumberFormatException
			//else :  there is no attribute with the attributeName, try next alternative
		}
		throw new SAXException("There is no attribute with name " + StringUtils.join(attributeNames, " or ") + ", this is required for tag " + qName + (reason != null ? " (" + reason + ")" : "") + ".");
	}
	
	/**
	 * Read an optional integer attribute with name {@code attributeName}, using the passed {@code attributes} collection.
	 * When no such attribute exists the {@code defaultValue} is returned.
	 * 
	 * @param attributeName
	 * @param defaultValue
	 * @param attributes
	 * @throws NumberFormatException when the attribute value string does not hold a valid integer (e.g. because it is empty)
	 * @return
	 */
	protected int readIntegerAttribute(String attributeName, int defaultValue, Attributes attributes) throws NumberFormatException
	{
		return readIntegerAttribute(defaultValue, attributes, attributeName);
	}

	/**
	 * Read an optional integer attribute with a name from {@code attributeNames} (tried in order, first existing attribute wins), using the passed {@code attributes} collection.
	 * When no such attribute exists the {@code defaultValue} is returned.
	 * 
	 * @param defaultValue
	 * @param attributes
	 * @param attributeNames	alternative attribute names ("synonyms")
	 * @throws NumberFormatException when the attribute value string does not hold a valid integer (e.g. because it is empty)
	 * @return
	 */
	protected int readIntegerAttribute(int defaultValue, Attributes attributes, String... attributeNames) throws NumberFormatException
	{
		for(String attributeName : attributeNames)
		{
			String strVal = attributes.getValue(attributeName);
			if(strVal == null)
				continue; // there is no attribute with the attributeName, try next alternative
			else
			{
				if(strVal.trim().isEmpty())
					return defaultValue;
				else
					return Integer.parseInt(strVal.trim()); // throws NumberFormatException
			}
		}
		return defaultValue;
	}

	/**
	 * Read an optional float attribute with name {@code attributeName}, using the passed {@code attributes} collection.
	 * When no such attribute exists the {@code defaultValue} is returned.
	 * 
	 * @param attributeName
	 * @param defaultValue
	 * @param attributes
	 * @throws NumberFormatException when the attribute value string does not hold a valid float (e.g. because it is empty)
	 * @return
	 */
	protected float readFloatAttribute(String attributeName, float defaultValue, Attributes attributes) throws NumberFormatException
	{
		return readFloatAttribute(defaultValue, attributes, attributeName);
	}
	
	/**
	 * Read an optional float attribute with a name from {@code attributeNames} (tried in order, first existing attribute wins), using the passed {@code attributes} collection.
	 * When no such attribute exists the {@code defaultValue} is returned.
	 * 
	 * @param defaultValue
	 * @param attributes
	 * @param attributeNames	alternative attribute names ("synonyms")
	 * @throws NumberFormatException when the attribute value string does not hold a valid float (e.g. because it is empty)
	 * @return
	 */
	protected float readFloatAttribute(float defaultValue, Attributes attributes, String... attributeNames) throws NumberFormatException
	{
		for(String attributeName : attributeNames)
		{
			String strVal = attributes.getValue(attributeName);
			if(strVal == null)
				continue; // there is no attribute with the attributeName, try next alternative
			else
			{
				if(strVal.trim().isEmpty())
					return defaultValue;
				else
					return Float.parseFloat(strVal.trim()); // throws NumberFormatException
			}
		}
		return defaultValue;
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
