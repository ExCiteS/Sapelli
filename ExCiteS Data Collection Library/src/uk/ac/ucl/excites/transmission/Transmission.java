/**
 * 
 */
package uk.ac.ucl.excites.transmission;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;

/**
 * @author mstevens
 * 
 */
public abstract class Transmission
{

	protected boolean full = false;
	
	protected DateTime sentAt = null; //used only on sending side
	protected DateTime receivedAt = null; //used on receiving side, and TODO on sending side once we have acknowledgements working
	
	protected Settings settings;
	protected TransmissionClient modelProvider; //only used on the receiving side
	protected Schema schema;
	protected Set<Column<?>> columnsToFactorOut;
	protected Map<Column<?>, Object> factoredOutValues = null;
	protected List<Record> records;

	/**
	 * To be called at the sending side.
	 * 
	 * @param schema
	 * @param settings
	 */
	public Transmission(Schema schema, Settings settings)
	{
		this(schema, null, settings);
	}
	
	/**
	 * To be called at the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param settings
	 */
	public Transmission(Schema schema, Set<Column<?>> columnsToFactorOut, Settings settings)
	{
		this(); //!!!
		if(schema == null)
			throw new NullPointerException("Schema cannot be null on sending side.");
		this.schema = schema;
		setColumnsToFactorOut(columnsToFactorOut);
		this.settings = settings;
	}
	
	/**
	 * To be called at the receiving side.
	 * 
	 * @param modelProvider
	 * @param settings
	 */
	public Transmission(TransmissionClient modelProvider)
	{
		this(); //!!!
		if(modelProvider == null)
			throw new NullPointerException("TransmissionClient cannot be null on receiving side.");
		this.modelProvider = modelProvider;
	}
	
	private Transmission()
	{
		this.records = new ArrayList<Record>();
	}
	
	protected void setColumnsToFactorOut(Set<Column<?>> columnsToFactorOut)
	{
		if(columnsToFactorOut != null)
		{
			for(Column<?> c : columnsToFactorOut)
				if(schema.getColumnIndex(c) == Schema.UNKNOWN_COLUMN_INDEX)
					throw new IllegalArgumentException(c.toString() + " does not belong to the given schema.");
		}
		this.columnsToFactorOut = columnsToFactorOut; //may be null
	}
	
	public List<Record> getRecords()
	{
		return records;
	}
	
	public boolean addRecord(Record record) throws Exception
	{
		if(!record.isFilled())
			return false;
		
		if(columnsToFactorOut != null)
		{
			if(records.isEmpty())
			{
				factoredOutValues = new HashMap<Column<?>, Object>();
				//Store "factored out" values:
				for(Column<?> c : columnsToFactorOut)
					factoredOutValues.put(c, c.retrieveValue(record));
			}
			else
			{	//Check if factored out values are the same
				for(Column<?> c : columnsToFactorOut)
				{
					Object rValue = c.retrieveValue(record);
					if(factoredOutValues.get(c) == null)
					{
						if(rValue != null)
							throw new IllegalArgumentException("Non-matching factored out value in " + c.toString());
					}
					else
					{
						if(rValue == null || !rValue.equals(factoredOutValues.get(c)))
							throw new IllegalArgumentException("Non-matching factored out value in " + c.toString());
					}
				}
			}
		}
		
		//Add the record:
		records.add(record);
		
		//Try preparing messages:
		try
		{
			preparePayload();
		}
		catch(TransmissionCapacityExceededException tcee)
		{	//adding this record caused transmission capacity to be exceeded, so remove it and mark the transmission as full (unless there are no other records)
			records.remove(record);
			if(!records.isEmpty())
				full = true;
			return false;
		}
		
		//Messages were successfully prepared...
		//record.setTransmission(this); //set transmission on record
		return true;
	}
	
	public void send(TransmissionSender transmissionSender) throws Exception
	{
		//Some checks:
		if(isSent())
		{
			System.out.println("This transmission (& all of its parts) has already been sent.");
			return;
		}
		if(records.isEmpty())
			throw new IllegalStateException("Transmission has no records. Add at least 1 record before sending the transmission .");
		if(transmissionSender == null)
			throw new IllegalStateException("Please provide a non-null TransmissionSender instance.");
		
		// Set sendingAttemptedAt for all records:
		DateTime now = new DateTime();
		for(Record r : records)
			r.setSendingAttemptedAt(now);
		
		sendPayload(transmissionSender); // !!!
	}

	public void resend(TransmissionSender sender) throws Exception
	{
		//Clear early sentAt value (otherwise send() won't work):
		sentAt = null;
		
		//Resend:
		send(sender);
	}
	
	protected abstract void sendPayload(TransmissionSender transmissionSender) throws Exception;
	
	public void read() throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException
	{
		read(null, null);
	}
	
	public void read(Schema schemaToUse, Settings settingsToUse) throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException
	{
		//Some checks:
		if(!records.isEmpty())
		{
			if(schema == schemaToUse && settings == settingsToUse)
			{
				System.out.println("This SMSTransmission has already been received.");
				return;
			}
			else
				records.clear(); //Re-receiving with other schema/settings, so clear records
		}
		
		//Read payload:
		readPayload(schemaToUse, settingsToUse);
	}
	
	protected abstract void preparePayload() throws IOException, TransmissionCapacityExceededException;
	
	protected abstract void readPayload(Schema schemaToUse, Settings settingsToUse) throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException;
	
	public boolean isFull()
	{
		return full;
	}
	
	public boolean isEmpty()
	{
		return records.isEmpty();
	}
	
	public boolean isSent()
	{
		return sentAt != null;
	}
	
	protected void setSentAt(DateTime sentAt)
	{
		for(Record r : records)
		{
			r.setSent(true);
			r.setSendingAttemptedAt(sentAt);
		}
		this.sentAt = sentAt;
	}
	
	public DateTime getSentAt()
	{
		return sentAt;
	}

	public boolean isReceived()
	{
		return receivedAt != null;
	}

	public DateTime getReceivedAt()
	{
		return receivedAt;
	}
	
	public void setReceivedAt(DateTime receivedAt)
	{
		this.receivedAt = receivedAt;
	}
	
	public Schema getSchema()
	{
		return schema;
	}
	
	public Settings getSettings()
	{
		return settings;
	}
	
}
