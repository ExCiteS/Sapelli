/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class DecodeException extends Exception
{
	
	private static final long serialVersionUID = 2L;
	
	private long modelID;
	private List<Record> records; //records at least partially decoded
	
	public DecodeException(String message, Throwable cause, long modelID, List<Record> records)
	{
		super(message, cause);
		this.modelID = modelID;
		this.records = new ArrayList<Record>(records);
	}

	public void addRecord(Record record)
	{
		if(record != null)
			this.records.add(record);
	}

	/**
	 * @return the modelID
	 */
	public long getModelID()
	{
		return modelID;
	}

	/**
	 * @return the records
	 */
	public List<Record> getRecords()
	{
		return records;
	}
	
}
