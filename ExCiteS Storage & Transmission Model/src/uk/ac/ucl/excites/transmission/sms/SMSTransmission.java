/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.transmission.SchemaProvider;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;


/**
 * @author mstevens
 *
 */
public abstract class SMSTransmission extends Transmission
{
	
	static public final int ID_SIZE_BITS = Byte.SIZE;
	static public final int INITIAL_ID = 0;
	static public final IntegerRangeMapping ID_FIELD = IntegerRangeMapping.ForSize(INITIAL_ID, ID_SIZE_BITS);
	
	protected SMSAgent receiver;
	protected SMSAgent sender;
	protected SMSService smsService;
	protected SortedSet<Message> parts;

	protected Integer id = null;
	protected boolean full = false;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param id
	 * @param receiver
	 * @param smsService
	 */
	public SMSTransmission(Schema schema, byte id, SMSAgent receiver, SMSService smsService)
	{
		this(schema, null, id, receiver, smsService);
	}
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param id
	 * @param receiver
	 * @param smsService
	 */
	public SMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, int id, SMSAgent receiver, SMSService smsService)
	{
		super(schema, columnsToFactorOut);
		this.id = id;
		
		this.receiver = receiver;
		this.smsService = smsService;
		this.parts = new TreeSet<Message>(new Message.MessageComparator());
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param schemaProvider
	 */
	public SMSTransmission(SchemaProvider schemaProvider)
	{
		super(schemaProvider);
		this.parts = new TreeSet<Message>(new Message.MessageComparator());
	}
	
	@Override
	public boolean addRecord(Record record) throws Exception
	{
		if(!record.isFilled())
			return false;
		if(records.isEmpty())
		{	//Store "factored out" values:
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
		
		//Add the record:
		records.add(record);
		
		//Try preparing messages:
		try
		{
			prepareMessages();
		}
		catch(TransmissionCapacityExceededException tcee)
		{	//adding this record caused transmission capacity to be exceeded, so remove it and mark the transmission as full (unless there are no other records)
			records.remove(record);
			if(!records.isEmpty())
				full = true;
			return false;
		}
		
		//Messages were successfully prepared...
		record.setTransmission(this); //set transmission on record
		return true;
	}
	
	/**
	 * To be called on receiving side.
	 * 
	 * @param msg
	 */
	public void addPart(Message msg)
	{
		if(parts.isEmpty())
		{	//set transmission ID & sender based on those of the first received message:
			this.id = msg.getTransmissionID();
			this.sender = msg.getSender();
		}
		else
		{	//each following received message must have a matching transmission id, sender & partsTotal:
			if(id.intValue() != msg.getTransmissionID())
				throw new IllegalArgumentException("This message does not belong to the transmission (ID mismatch)");
			if(!sender.equals(msg.getSender()))
				throw new IllegalArgumentException("This message originates from another sender.");
		}
		//TODO deal with duplicates!
		parts.add(msg);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @return whether all parts have been received
	 */
	public boolean isComplete()
	{
		if(parts.isEmpty())
			return false;
		return (parts.first().getTotalParts() == parts.size());
	}
	
	protected void prepareMessages() throws IOException, TransmissionCapacityExceededException
	{
		//Encode
		byte[] data = encodeRecords(); //can throw IOException
		
		//Compress
		data = compress(data);
		
		//Encrypt
		data = encrypt(data);
		
		//Serialise/Split
		List<Message> msgs = serialiseAndSplit(data); //can throw TransmissionCapacityExceededException
		for(Message m : msgs)
			parts.add(m);
	}
	
	protected void readMessages() throws IOException
	{
		if(parts.isEmpty())
			throw new IllegalStateException("No messages to decode.");
		if(!isComplete())
			throw new IllegalStateException("Transmission is incomplete, " + (parts.first().getTotalParts() - parts.size()) + " parts missing.");
		
		//Merge/Deserialise
		byte[] data = mergeAndDeserialise(parts);
		
		//Decrypt
		data = decrypt(data);
		
		//Decompress
		data = decompress(data);
		
		//Decode
		decodeRecords(data);
	}

	@Override
	public void send() throws Exception
	{
		//Prepare messages:
		prepareMessages();
		//Send them one by one:
		for(Message m : parts)
			m.send();
	}
	
	protected byte[] encodeRecords() throws IOException
	{
		BitOutputStream out = null;
		try
		{
			//Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
	
			//Write schema ID & version:
			Schema.SCHEMA_ID_FIELD.write(schema.getID(), out);
			Schema.SCHEMA_VERSION_FIELD.write(schema.getVersion(), out);
			
			//Write factored out values:
			for(Column<?> c : columnsToFactorOut)
				c.writeObject(factoredOutValues.get(c), out);
			
			//TODO write number of records?
			
			//Write records:
			for(Record r : records)
				r.writeToBitStream(out, columnsToFactorOut);
			
			//Flush, close & get bytes:
			out.flush();
			out.close();
	
			return rawOut.toByteArray();
		}
		catch(Exception e)
		{
			throw new IOException("Error on encoding records.", e);
		}
		finally
		{
			try
			{
				if(out != null)
					out.close();
			}
			catch(Exception ignore) {}
		}
	}
	
	protected void decodeRecords(byte[] data) throws IOException
	{
		BitInputStream in = null;
		try
		{
			//Input stream:
			in = new BitInputStream(new ByteArrayInputStream(data));

			//Read schema ID & version
			int schemaID = (int) Schema.SCHEMA_ID_FIELD.read(in);
			int schemaVersion = (int) Schema.SCHEMA_VERSION_FIELD.read(in);
			
			//Look-up schema & columns to factor out:
			schema = schemaProvider.getSchema(schemaID, schemaVersion);
			if(schema == null)
				throw new IllegalStateException("Cannot decode message because schema (id: " + schemaID + "; version: " + schemaVersion + ") is unknown");
			setColumnsToFactorOut(schemaProvider.getFactoredOutColumnsFor(schema));
			
			//Read factored out values:
			for(Column<?> c : columnsToFactorOut)
				factoredOutValues.put(c, c.readValue(in));
			
			//TODO read number of records?
			
			//Read records:
			while(!in.atEnd())
			{
				Record r = new Record(schema);
				r.readFromBitStream(in, columnsToFactorOut);
				//Set factored out values:
				//Read factored out values:
				for(Column<?> c : columnsToFactorOut)
					c.storeObject(r, factoredOutValues.get(c));
				records.add(r);
			}
		}
		catch(Exception e)
		{
			throw new IOException("Error on decoding records.", e);
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(Exception ignore) {}
		}
	}
	
	protected byte[] compress(byte[] data)
	{
		//TODO compression
		
		return data;
	}
	
	protected byte[] decompress(byte[] data)
	{
		//TODO decompression
		
		return data;
	}

	protected byte[] encrypt(byte[] data)
	{
		//TODO encryption
		
		return data;
	}
	
	protected byte[] decrypt(byte[] data)
	{
		//TODO decryption
		
		return data;
	}
	
	protected abstract List<Message> serialiseAndSplit(byte[] data) throws TransmissionCapacityExceededException;
	
	protected abstract byte[] mergeAndDeserialise(Set<Message> parts);
	
	public boolean isFull()
	{
		return full;
	}

	public int getID()
	{	
		if(id == null)
			throw new NullPointerException("Transmission ID has not been set.");
		return id.intValue();
	}

	public void resend(int partNumber)
	{
		
	}
	
	public void partSent(Message smsMessage)
	{
		boolean allSent = true;
		DateTime lastSentAt = null;
		for(Message m : parts)
		{
			if(!m.isSent())
			{
				allSent = false;
				break;
			}
			else
			{
				if(lastSentAt == null || lastSentAt.isBefore(m.getSentAt()))
					lastSentAt = m.getSentAt();
			}
		}
		if(allSent)
			sentAt = lastSentAt;
	}

	public void partReceived(Message smsMessage)
	{
		boolean allReceived = true;
		DateTime lastReceivedAt = null;
		for(Message m : parts)
		{
			if(!m.isReceived())
			{
				allReceived = false;
				break;
			}
			else
			{
				if(lastReceivedAt == null || lastReceivedAt.isBefore(m.getReceivedAt()))
					lastReceivedAt = m.getReceivedAt();
			}
		}
		if(allReceived)
			receivedAt = lastReceivedAt;		
	}
	
	public SMSService getSMSService()
	{
		return smsService;
	}
	
}
