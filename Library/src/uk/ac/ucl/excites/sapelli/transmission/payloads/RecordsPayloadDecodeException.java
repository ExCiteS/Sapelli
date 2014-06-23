/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.payloads;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;

/**
 * @author mstevens
 *
 */
public class RecordsPayloadDecodeException extends PayloadDecodeException
{
	
	private static final long serialVersionUID = 2L;
		
	/**
	 * Partially decoded records (not stored in the RecordPayload itself)
	 */
	private List<Record> partialRecords;
	
	public RecordsPayloadDecodeException(RecordsPayload payload, String message, Throwable cause)
	{
		super(payload, message, cause);
		this.partialRecords = new ArrayList<Record>(partialRecords);
	}
	
	public RecordsPayloadDecodeException(RecordsPayload payload, String message)
	{
		this(payload, message, null);
	}

	public void addPartialRecord(Record record)
	{
		if(record != null)
			this.partialRecords.add(record);
	}

	/**
	 * @return the modelID
	 */
	public long getModelID()
	{
		return ((RecordsPayload) payload).model.getID();
	}

	/**
	 * @return the records
	 */
	public List<Record> getRecords()
	{
		List<Record> records = new ArrayList<Record>();
		records.addAll(((RecordsPayload) payload).getRecords());
		records.addAll(partialRecords);
		return records;
	}
	
}
