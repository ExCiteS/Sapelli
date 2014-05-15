/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Abstract superclass for all Transmissions
 * 
 * TODO support for multi-schema transmissions? (containing records of more than 1 schema)
 * 
 * @author mstevens
 */
public abstract class Transmission
{

	protected boolean full = false;
	
	protected DateTime sentAt = null; //used only on sending side
	protected DateTime receivedAt = null; //used on receiving side, and TODO on sending side once we have acknowledgements working
	
	protected Settings settings;
	protected TransmissionClient client; //only used on the receiving side
	protected long modelID = -1;
	protected final Map<Schema,List<Record>> recordsBySchema;
	protected Set<Column<?>> columnsToFactorOut;
	protected Map<Column<?>, Object> factoredOutValues = null;

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
	public Transmission(Settings settings)
	{
		this(); //!!!
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
		this.client = modelProvider;
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
		setColumnsToFactorOut(columnsToFactorOut);
		this.settings = settings;
	}
	
	private Transmission()
	{
		this.recordsBySchema = new HashMap<Schema, List<Record>>();
	}
	
	protected void setColumnsToFactorOut(Set<Column<?>> columnsToFactorOut)
	{
//		if(columnsToFactorOut == null)
//			columnsToFactorOut = Collections.<Column<?>>emptySet();
//		else
//			for(Column<?> c : columnsToFactorOut)
//				if(!schema.containsColumn(c.getName(), false)) 
//					throw new IllegalArgumentException("Column \"" + c.toString() + "\" does not belong to the given schema.");
		this.columnsToFactorOut = columnsToFactorOut;
	}
	
	/**
	 * @return records grouped by schema
	 */
	public Map<Schema,List<Record>> getRecordsBySchema()
	{
		return recordsBySchema;
	}
	
	/**
	 * @return flat list of records (sorted by Schema)
	 */
	public List<Record> getRecords()
	{
		List<Record> allRecords = new ArrayList<Record>();
		for(Entry<Schema, List<Record>> entry : recordsBySchema.entrySet())
			allRecords.addAll(entry.getValue());
		return allRecords;
	}
	
	/**
	 * @return all schemata for which the transmission contains records 
	 */
	public Set<Schema> getSchemata()
	{
		return recordsBySchema.keySet();
	}
	
	/**
	 * @return whether the transmission contains records of more than 1 schema
	 */
	public boolean isMultiSchema()
	{
		return recordsBySchema.keySet().size() > 1;
	}
	
	/**
	 * To be called from the sending side
	 * 
	 * @param record
	 * @return
	 * @throws Exception
	 */
	public boolean addRecord(Record record) throws Exception
	{
		if(!record.isFilled())
			return false; // record is not fully filled (non-optional values are still null)
		Schema schema = record.getSchema();
		if(schema.isInternal())
			throw new IllegalArgumentException("Cannot directly transmit records of internal schema.");
		if(recordsBySchema.isEmpty())
			// set model ID:
			modelID = schema.getModelID();
		//	Check model ID:
		else if(modelID != schema.getModelID())
			throw new IllegalArgumentException("The schemata of the records in a single Transmission must all belong to the same model.");

		// Add the record:
		List<Record> recordsOfSchema = recordsBySchema.get(schema);
		if(recordsOfSchema == null)
		{
			recordsOfSchema = new ArrayList<Record>();
			recordsBySchema.put(schema, recordsOfSchema);
		}
		recordsOfSchema.add(record);
		
		// Try preparing messages:
		try
		{
			preparePayload();
		}
		catch(TransmissionCapacityExceededException tcee)
		{	// adding this record caused transmission capacity to be exceeded, so remove it and mark the transmission as full (unless there are no other records)
			recordsOfSchema.remove(record);
			if(recordsOfSchema.isEmpty())
				recordsBySchema.remove(schema);
			if(!recordsBySchema.isEmpty())
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
		if(recordsBySchema.isEmpty())
			throw new IllegalStateException("Transmission has no records. Add at least 1 record before sending the transmission .");
		if(transmissionSender == null)
			throw new IllegalStateException("Please provide a non-null TransmissionSender instance.");
		
		// Set sendingAttemptedAt for all records:
//		DateTime now = new DateTime();
//		for(Record r : records)
//			r.setSendingAttemptedAt(now);
		
		sendPayload(transmissionSender); // !!!
	}

	public void resend(TransmissionSender sender) throws Exception
	{
		// Clear early sentAt value (otherwise send() won't work):
		sentAt = null;
		
		// Resend:
		send(sender);
	}
	
	protected abstract void sendPayload(TransmissionSender transmissionSender) throws Exception;
	
	public void receive() throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException
	{
		// Some checks:
		if(!recordsBySchema.isEmpty())
			throw new IllegalStateException("This SMSTransmission has already been received.");
		
		// Read payload:
		receivePayload(null, null);
	}
	
	protected abstract void preparePayload() throws IOException, TransmissionCapacityExceededException;
	
	protected abstract void receivePayload(Schema schemaToUse, Settings settingsToUse) throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException;
	
	public boolean isFull()
	{
		return full;
	}
	
	public boolean isEmpty()
	{
		return recordsBySchema.isEmpty();
	}
	
	public boolean isSent()
	{
		return sentAt != null;
	}
	
	protected void setSentAt(DateTime sentAt)
	{
		for(List<Record> records : recordsBySchema.values())
			for(Record r : records)
			{
				r.setSent(true);
				//r.setSendingAttemptedAt(sentAt);
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
	
	public long getModelID()
	{
		return modelID;
	}
	
	public Settings getSettings()
	{
		return settings;
	}
	
}
