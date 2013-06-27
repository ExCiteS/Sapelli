/**
 * 
 */
package uk.ac.ucl.excites.storage.xml;

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

import uk.ac.ucl.excites.collector.database.DataAccess;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.util.XMLParser;

/**
 * @author mstevens
 * 
 */
public class RecordsImporter extends XMLParser
{

	private DataAccess dao;
	private Record currentRecord;
	private Column<?> currentColumn;
	private List<Record> records;

	public RecordsImporter(DataAccess dao)
	{
		super();
		this.dao = dao;
	}

	public List<Record> importFrom(File xmlFile) throws Exception
	{
		if(xmlFile == null || !xmlFile.exists() || xmlFile.length() == 0)
			throw new IllegalArgumentException("Invalid xmlFile (" + (xmlFile == null ? "null" : xmlFile.getAbsolutePath()) + ")!");
		InputStream input = new FileInputStream(xmlFile);
		records = new ArrayList<Record>();
		try
		{
			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();
			XMLReader xr = sp.getXMLReader();
			xr.setContentHandler(this);
			xr.parse(new InputSource(input));
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
		return records;
	}

	@Override
	public void startDocument() throws SAXException
	{
		// does nothing (for now)
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		// <Record>
		if(qName.equals(Record.TAG_RECORD))
		{
			int schemaID = Integer.parseInt(readRequiredStringAttribute(Record.TAG_RECORD, attributes, Record.ATTRIBUTE_FORM_SCHEMA_ID));
			int schemaVersion = Integer.parseInt(readRequiredStringAttribute(Record.TAG_RECORD, attributes, Record.ATTRIBUTE_FORM_SCHEMA_VERSION));
			Schema schema = dao.retrieveSchema(schemaID, schemaVersion);
			if(schema == null)
				warnings.add("Record skipped because schema with ID " + schemaID + " and version " + schemaVersion + " is unknown, please load the appropriate project.");
			else
				currentRecord = new Record(schema);
		}
		// TODO transmission? sent/received
		/*else if(qName.equals("???"))
		{
			
		}*/
		// Record columns:
		else if(currentRecord != null)
		{
			Schema s = currentRecord.getSchema();
			currentColumn = s.getColumn(qName);
			if(currentColumn == null)
				warnings.add("Column " + qName + " does not exist in schema with ID " + s.getID() + " and version " + s.getVersion());
		}
	}

	@Override
	public void characters(char ch[], int start, int length) throws SAXException
	{
		if(currentRecord != null && currentColumn != null)
		{
			try
			{
				currentColumn.parseAndStoreValue(currentRecord, new String(ch, start, length));
			}
			catch(Exception e)
			{
				warnings.add("Error upon parsing value (" + new String(ch, start, length) + "): " + e.getMessage());
			}
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException
	{
		// </Record>
		if(qName.equals(Record.TAG_RECORD))
		{
			records.add(currentRecord);
			currentRecord = null;
		}
		// Record columns:
		else if(currentRecord != null && currentColumn != null)
		{
			currentColumn = null;
		}
	}

	@Override
	public void endDocument() throws SAXException
	{
		// does nothing for now
	}

}
