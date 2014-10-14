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

import java.io.File;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.zip.CRC32;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import uk.ac.ucl.excites.sapelli.shared.util.xml.DocumentParser;

/**
 * Computes hash codes from an XML file or inputstream, ignoring white space and comments
 * 
 * Generated hashes should be consistent across platforms (tested on Android v4.4 and Java8/Windows)
 * 
 * @author mstevens
 */
public class XMLHasher extends DocumentParser
{

	static private final Charset UTF8 = Charset.forName("UTF-8");
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
		return getJavaHashCode(open(xmlFile));
	}

	/**
	 * Returns a signed 32bit hash code, computed in "Java-style" (i.e. similarly to {@link Object#hashCode()})
	 * 
	 * @param input
	 * @return
	 * @throws Exception
	 */
	public int getJavaHashCode(InputStream input) throws Exception
	{
		try
		{
			parse(input);
			return hashCode;
		}
		finally
		{	// Reset:
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
		return getCRC32HashCode(open(xmlFile));
	}

	/**
	 * Returns an unsigned 32bit hash code, computed using CRC32
	 * 
	 * @param input
	 * @return
	 * @throws Exception
	 */
	public long getCRC32HashCode(InputStream input) throws Exception
	{
		try
		{
			crc = new CRC32();
			parse(input);
			return crc.getValue();
		}
		finally
		{	// Reset:
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
			{
				if(str.isEmpty())
					crc.update(0);
				else
					crc.update(str.getBytes(UTF8));
			}
		}
	}
	
	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		// Tag
		update(qName);
		
		// Attributes
		for(int i = 0; i < attributes.getLength(); i++)
		{
			update(attributes.getQName(i));
			update(attributes.getValue(i));
		}
	}
	
	/**
	 * Character chunks are trimmed and only is the result is non-empty they are used in the hash computation.
	 * Note: the difference with handling attributes values ({@see #startElement(String, String, String, Attributes)}),
	 * which are not trimmed and included when empty, is deliberate. We do this because on Android more whitespace-only
	 * character chunks tend to be generated than on desktop java, which causes different hashes to be computed. 
	 * 
	 * @see uk.ac.ucl.excites.sapelli.shared.util.xml.Handler#characters(char[], int, int)
	 */
	@Override
	public void characters(char ch[], int start, int length) throws SAXException
	{
		String charblock = new String(ch, start, length).trim(); // always trim "inter-tag" char blocks ...
		if(!charblock.isEmpty()) // and do not include them when empty
			update(charblock);
	}

}
