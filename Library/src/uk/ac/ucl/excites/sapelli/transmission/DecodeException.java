/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class DecodeException extends Exception
{
	
	private static final long serialVersionUID = 9169900654612685777L;
	
	private Schema schema;
	private List<Record> records; //records at least partially decoded
	
	public DecodeException(String message, Throwable cause, Schema schema, List<Record> records)
	{
		super(message, cause);
		this.schema = schema;
		this.records = new ArrayList<Record>(records);
	}

	public void addRecord(Record record)
	{
		if(record != null)
			this.records.add(record);
	}

	/**
	 * @return the schema
	 */
	public Schema getSchema()
	{
		return schema;
	}

	/**
	 * @return the records
	 */
	public List<Record> getRecords()
	{
		return records;
	}
	
}
