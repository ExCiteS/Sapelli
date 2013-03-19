/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;


/**
 * @author mstevens
 *
 */
public abstract class SMSTransmission extends Transmission
{
	
	static public final int ID_LENGTH_BITS = Byte.SIZE;
	
	protected SMSAgent receiver;
	protected SMSAgent sender;
	protected SMSService smsService;
	protected List<Message> parts;

	protected byte id;
	protected boolean full = false;
	
	public SMSTransmission(Schema schema, byte id, SMSAgent receiver, SMSService smsService)
	{
		this(schema, new HashSet<Column<?>>(), id, receiver, smsService);
	}
	
	public SMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, byte id, SMSAgent receiver, SMSService smsService)
	{
		super(schema, columnsToFactorOut);
		this.id = id;
		
		this.receiver = receiver;
		this.smsService = smsService;
		this.parts = new ArrayList<Message>();
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
	
	public void addPart(Message msg)
	{
		if(msg.getTransmissionID() != id)
			throw new IllegalArgumentException("This message does not belong to the transmission (ID mismatch)");
		//TODO store msg in order
	}
	
	protected void prepareMessages() throws IOException, TransmissionCapacityExceededException
	{
		//Encode
		byte[] data = encodeRecords();
		
		//Compress
		data = compress(data);
		
		//Encrypt
		data = encrypt(data);
		
		//Serialise/Split
		parts = serialiseAndSplit(data);
	}
	
	protected void readMessages() throws IOException
	{
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
	
	protected abstract byte[] mergeAndDeserialise(List<Message> parts);
	
	public boolean isFull()
	{
		return full;
	}

	public byte getID()
	{
		return id;
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
