/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.xml;

import java.io.File;
import java.util.ArrayList;
import java.util.List;





import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.util.xml.DocumentParser;

/**
 * @author mstevens
 * 
 */
public class RecordsImporter extends DocumentParser
{

	protected StorageClient client;
	protected Record currentRecord;
	protected Column<?> currentColumn;
	protected List<Record> records;

	public RecordsImporter(StorageClient client)
	{
		super();
		this.client = client;
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
			if(currentRecord != null)
				throw new SAXException("Records cannot be nested!");
			
			Schema schema = null;
			String schemaDescr = null;
			if(attributes.getIndex(Schema.V1X_ATTRIBUTE_SCHEMA_ID) != -1)
			{	//This file contains records exported by Sapelli v1.x
				int schemaID = readRequiredIntegerAttribute(Record.TAG_RECORD, Schema.V1X_ATTRIBUTE_SCHEMA_ID, "because this is a v1.x record", attributes);
				int schemaVersion = readIntegerAttribute(Schema.V1X_ATTRIBUTE_SCHEMA_VERSION, Schema.V1X_DEFAULT_SCHEMA_VERSION, attributes);
				schema = client.getSchemaV1(schemaID, schemaVersion);
				schemaDescr = "ID " + schemaID + " and version " + schemaVersion;
			}
			else
			{
				int usageID = readRequiredIntegerAttribute(Record.TAG_RECORD, Schema.ATTRIBUTE_USAGE_ID, attributes);
				int usageSubID = readIntegerAttribute(Schema.ATTRIBUTE_USAGE_ID, Schema.DEFAULT_USAGE_SUB_ID, attributes);
				schema = client.getSchema(usageID, usageSubID);
				schemaDescr = "usageID " + usageID + " and usageSubID " + usageSubID;
			}
			if(schema == null)
				addWarning("Record skipped because schema with " + schemaDescr + " is unknown, please load the appropriate project.");
			else
				currentRecord = client.getNewRecord(schema);
			// TODO transmission? sent/received
		}
		// Record columns:
		else if(currentRecord != null)
		{
			Schema s = currentRecord.getSchema();
			currentColumn = s.getColumn(qName);
			if(currentColumn == null)
				addWarning("Column " + qName + " does not exist in " + s.toString());
		}
		// <?>
		else
			addWarning("Ignored unrecognised or invalidly placed element \"" + qName + "\".");
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
				addWarning("Error upon parsing value (" + new String(ch, start, length) + "): " + e.getMessage());
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
