/**
 * 
 */
package uk.ac.ucl.excites.transmission;

import java.util.ArrayList;
import java.util.List;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 * 
 */
public abstract class Transmission
{

	protected DateTime sentAt = null;
	protected DateTime receivedAt = null;
	protected DateTime confirmationSentAt = null;
	protected DateTime confirmationReceivedAt = null;
	
	protected Schema schema;
	protected List<Record> records;
	

	public Transmission(Schema schema)
	{
		this.schema = schema;
		this.records = new ArrayList<Record>();
	}
	
	public abstract boolean addRecord(Record record);
	
	public List<Record> getRecords()
	{
		return records;
	}
	
	public abstract void send();
	
	public boolean isEmpty()
	{
		return records.size() == 0;
	}
	
	public boolean isSent()
	{
		return sentAt != null;
	}

	public boolean isReceived()
	{
		return receivedAt != null;
	}
	
}
