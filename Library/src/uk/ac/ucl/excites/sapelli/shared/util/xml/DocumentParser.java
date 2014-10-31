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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

/**
 * @author mstevens
 * 
 */
public abstract class DocumentParser extends Handler
{
	
	protected InputStream open(File xmlFile) throws Exception
	{
		if(xmlFile == null || !xmlFile.exists() || xmlFile.length() == 0)
			throw new IllegalArgumentException("Invalid or non-existant XML file (" + (xmlFile == null ? "null" : xmlFile.getAbsolutePath()) + ")!");
		return new FileInputStream(xmlFile);
	}

	protected void parse(InputStream input) throws Exception
	{
		if(input == null)
			throw new IllegalArgumentException("Invalid input stream");
		clearWarnings();
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
			throw e;
		}
		finally
		{	// In case the stream is still open:
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

}
