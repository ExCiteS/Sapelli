/**
 * 
 */
package uk.ac.ucl.excites.sapelli.util.xml;

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
