/**
 * 
 */
package uk.ac.ucl.excites.aggregator;

import org.xml.sax.SAXException;

import uk.ac.ucl.excites.collector.database.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Form;

import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.storage.xml.RecordsImporter;

/**
 * @author mstevens
 * 
 */
public class CustomRecordsImporter extends RecordsImporter
{

	private Aggregator aggregator;

	public CustomRecordsImporter(DataAccess dao, Aggregator aggregator)
	{
		super(dao);
		this.aggregator = aggregator;
	}

	@Override
	public void characters(char ch[], int start, int length) throws SAXException
	{
		if(currentRecord != null && currentColumn != null)
		{
			try
			{
				currentColumn.parseAndStoreValue(currentRecord, new String(ch, start, length));
				if(currentColumn.getName().equals(Form.COLUMN_TIMESTAMP_START)) //!!!
				{
					Schema correctSchema = aggregator.getCorrectSchema(currentRecord);
					if(correctSchema != currentRecord.getSchema())
					{	//Dirty hack but it works (because stratTime is the first column we only need to deal with that value)
						currentRecord = new Record(correctSchema);
						currentColumn = correctSchema.getColumn(Form.COLUMN_TIMESTAMP_START);
						currentColumn.parseAndStoreValue(currentRecord, new String(ch, start, length));
						//System.out.println("Schema replaced.");
					}
				}
			}
			catch(Exception e)
			{
				warnings.add("Error upon parsing value (" + new String(ch, start, length) + "): " + e.getMessage());
			}
		}
	}

}
