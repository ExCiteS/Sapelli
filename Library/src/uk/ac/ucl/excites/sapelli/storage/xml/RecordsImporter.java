/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.xml;

import java.io.File;
import java.util.ArrayList;
import java.util.List;



import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import uk.ac.ucl.excites.sapelli.collector.database.DataAccess;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.util.XMLParser;

/**
 * @author mstevens
 * 
 */
public class RecordsImporter extends XMLParser
{

	protected DataAccess dao;
	protected Record currentRecord;
	protected Column<?> currentColumn;
	protected List<Record> records;

	public RecordsImporter(DataAccess dao)
	{
		super();
		this.dao = dao;
	}

	public List<Record> importFrom(File xmlFile) throws Exception
	{
		records = new ArrayList<Record>();
		parse(open(xmlFile));
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
