/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.shared.util.xml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper;


/**
 * @author mstevens
 *
 */
public abstract class Handler extends DefaultHandler implements WarningKeeper
{

	// Static
	protected static final String ENABLED = "enabled";
	protected static final String DISABLED = "disabled";
	
	// Dynamic
	private final Map<String, SubtreeParser<?>> subtreeParsers;
	private SubtreeParser<?> activeSubtreeParser;
	
	private List<String> warnings;

	public Handler()
	{
		this.subtreeParsers = new HashMap<String, SubtreeParser<?>>();
		this.activeSubtreeParser = null;
	}
	
	public void addSubtreeParser(SubtreeParser<?> subtreeParser)
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
	
	public void activateSubtreeParser(SubtreeParser<?> subtreeParser)
	{
		if(subtreeParsers.get(subtreeParser.getRootElementQName()) == null)
			throw new IllegalArgumentException("Unknown SubtreeParser");
		this.activeSubtreeParser = subtreeParser;
	}
	
	public void deactivateSubtreeParser(SubtreeParser<?> subtreeParser)
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
	public SubtreeParser<?> getActiveSubtreeParser()
	{
		return activeSubtreeParser;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		XMLAttributes tagAttributes = new XMLAttributes(attributes);
		try
		{
			// Try delegating to active SubtreeParser:
			if(activeSubtreeParser != null)
				activeSubtreeParser.parseStartElement(uri, localName, qName, tagAttributes);
			// Try delegating to subtree parser with matching root element:
			else if(subtreeParsers.containsKey(qName))
				subtreeParsers.get(qName).parseStartElement(uri, localName, qName, tagAttributes); //delegate to subtree parser with matching root element, it will activate itself if successful
			// Handle locally:
			else
				this.parseStartElement(uri, localName, qName, tagAttributes);
		}
		catch(Exception e)
		{
			throw new SAXException(e);
		}
	}
	
	/**
	 * To be overridden if necessary
	 * 
	 * @param uri
	 * @param localName
	 * @param qName
	 * @param attributes
	 * @throws Exception
	 */
	protected void parseStartElement(String uri, String localName, String qName, XMLAttributes attributes) throws Exception
	{
		super.startElement(uri, localName, qName, attributes.getAttributes());
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

	@Override
	public void addWarning(String warning)
	{
		if(warnings == null)
			warnings = new ArrayList<String>();
		warnings.add(warning);
	}

	@Override
	public void addWarnings(Collection<String> warnings)
	{
		if(this.warnings == null)
			this.warnings = new ArrayList<String>();
		this.warnings.addAll(warnings);
	}

	@Override
	public List<String> getWarnings()
	{
		return warnings != null ? warnings : Collections.<String> emptyList();
	}
	
	@Override
	public void clearWarnings()
	{
		warnings = null;
	}

}
