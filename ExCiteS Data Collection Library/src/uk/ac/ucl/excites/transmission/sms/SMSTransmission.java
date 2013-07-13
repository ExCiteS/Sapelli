/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.DecodeException;
import uk.ac.ucl.excites.transmission.ModelProvider;
import uk.ac.ucl.excites.transmission.TransmissionSender;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.compression.CompressorFactory;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;


/**
 * @author mstevens
 *
 */
public abstract class SMSTransmission extends Transmission
{
	
	protected SMSAgent receiver;
	protected SMSAgent sender;
	protected SortedSet<Message> parts;
	protected DateTime deliveredAt;
	
	protected Integer id = null;
	protected boolean full = false;
	
	protected float compressionRatio = 1.0f;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param id
	 * @param receiver
	 * @param settings
	 */
	public SMSTransmission(Schema schema, int id, SMSAgent receiver, Settings settings)
	{
		this(schema, null, id, receiver, settings);
	}
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param id
	 * @param receiver
	 * @param settings
	 */
	public SMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, int id, SMSAgent receiver, Settings settings)
	{
		super(schema, columnsToFactorOut, settings);
		this.id = id;
		
		this.receiver = receiver;
		this.parts = new TreeSet<Message>();
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param modelProvider
	 * @param settings
	 */
	public SMSTransmission(ModelProvider modelProvider)
	{
		super(modelProvider);
		this.parts = new TreeSet<Message>();
	}
	
	@Override
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
		if(!parts.contains(msg)) //check for duplicates
		{
			parts.add(msg);
			msg.setTransmission(this);
		}
	}
	
	public SortedSet<Message> getParts()
	{
		return parts;
	}
	
	public int getCurrentNumberOfParts()
	{
		return parts.size();
	}
	
	public int getTotalNumberOfParts()
	{
		return parts.first().getTotalParts(); // Do not use parts.size() because that is not correct for incomplete transmissions on the receiving side
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
	
	public void send(TransmissionSender transmissionSender) throws Exception
	{
		//Some checks:
		if(isSent())
		{
			System.out.println("All parts of this SMSTransmission have already been sent.");
			return;
		}
		if(records.isEmpty())
			throw new IllegalStateException("Transmission has no records. Add at least 1 record before sending the transmission .");
		if(transmissionSender == null)
			throw new IllegalStateException("Please provide a non-null TransmissionSender instance.");
		
		//Prepare messages:
		prepareMessages();
		
		//Send unsent messages one by one:
		for(Message m : parts)
			if(!m.isSent())
				m.send(transmissionSender.getSMSService());
	}
	
	public void receive() throws IllegalStateException, IOException, DecodeException
	{
		receive(null, null);
	}
	
	public void receive(Schema schemaToUse, Settings settingsToUse) throws IllegalStateException, IOException, DecodeException
	{
		//Some checks:
		if(isReceived())
		{
			if(schema == schemaToUse && settings == settingsToUse)
			{
				System.out.println("This SMSTransmission has already been received.");
				return;
			}
			else
				records.clear(); //Re-receiving with other schema/settings, so clear records
		}
		
		if(parts.isEmpty())
			throw new IllegalStateException("No messages to decode.");
		if(!isComplete())
			throw new IllegalStateException("Transmission is incomplete, " + (parts.first().getTotalParts() - parts.size()) + " parts missing.");
		
		//Read messages:
		readMessages(schemaToUse, settingsToUse);
		
		//On successful reading of messages:
		if(!records.isEmpty())
			receivedAt = new DateTime(); //= now	
	}
	
	protected void prepareMessages() throws IOException, TransmissionCapacityExceededException
	{
		BitOutputStream out = null;
		try
		{
			// Encode records
			byte[] data = encodeRecords(); //can throw IOException
			//System.out.println("Encoded records to: " + data.length + " bytes");
			//System.out.println("Hash: " + BinaryHelpers.toHexadecimealString(Hashing.getSHA256Hash(data)));
			
			// Compress records
			data = compress(data);
			
			// Encrypt records
			data = encrypt(data);
			
			// Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
			
			// Write schema ID & version:
			Schema.SCHEMA_ID_FIELD.write(schema.getID(), out);
			Schema.SCHEMA_VERSION_FIELD.write(schema.getVersion(), out);
			
			// Write the encoded, compressed & encrypted records:
			out.write(data);
			
			// Flush & close the stream and get bytes:
			out.flush();
			out.close();
			data = rawOut.toByteArray();

			// Serialise/Split
			parts.clear();  //clear previous messages
			serialiseAndSplit(data); //can throw TransmissionCapacityExceededException
		}
		catch(IOException e)
		{
			throw new IOException("Error on preparing messages.", e);
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
	
	/**
	 * @param schemaToUse - can be null
	 * @param settingsToUse - can be null
	 * @throws IllegalStateException
	 * @throws IOException
	 * @throws DecodeException
	 */
	protected void readMessages(Schema schemaToUse, Settings settingsToUse) throws IllegalStateException, IOException, DecodeException
	{
		BitInputStream in = null;
		try
		{
			// Merge/Deserialise
			byte[] data = mergeAndDeserialise();
		
			// Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(data);
			in = new BitInputStream(rawIn);
			
			// Read schema ID & version:
			int schemaID = (int) Schema.SCHEMA_ID_FIELD.read(in);
			int schemaVersion = (int) Schema.SCHEMA_VERSION_FIELD.read(in);
			//if(schemaToUse != null)
			//	System.out.println("Using provided schema (ID: " + schemaToUse.getID() + "; version: " + schemaToUse.getVersion() + ") instead of the one indicated by the transmission (ID: " + schemaID + "; version: " + schemaVersion + ").");
			
			// Look-up schema unless one was provided:
			schema = (schemaToUse == null ? modelProvider.getSchema(schemaID, schemaVersion) : schemaToUse);
			if(schema == null)
				throw new IllegalStateException("Cannot decode message because schema (ID: " + schemaID + "; version: " + schemaVersion + ") is unknown");
			
			// Look-up settings & columns to factor out:
			settings = (settingsToUse == null ? modelProvider.getSettingsFor(schema) : settingsToUse);
			setColumnsToFactorOut(modelProvider.getFactoredOutColumnsFor(schema));
			
			// Read encrypted, compressed & encoded records:
			data = in.readBytes(in.available());
			
			// Decrypt records
			data = decrypt(data);
			
			// Decompress records
			data = decompress(data);
			
			// Decode records
			//System.out.println("Decoding records from: " + data.length + " bytes");
			//System.out.println("Hash: " + BinaryHelpers.toHexadecimealString(Hashing.getSHA256Hash(data)));
			decodeRecords(data);
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
	
	protected byte[] encodeRecords() throws IOException
	{
		BitOutputStream out = null;
		try
		{
			//Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
			
			//Write factored out values:
			if(columnsToFactorOut != null)
				for(Column<?> c : columnsToFactorOut)
					c.writeObject(factoredOutValues.get(c), out);
			
			//Write records:
			for(Record r : records)
				r.writeToBitStream(out, columnsToFactorOut);
			
			//Flush & close the stream and get bytes:
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
	
	protected void decodeRecords(byte[] data) throws DecodeException
	{
		BitInputStream in = null;
		Record r = null;
		try
		{
			//Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(data);
			in = new BitInputStream(rawIn);
			
			//Read factored out values:
			if(columnsToFactorOut != null)
			{
				factoredOutValues = new HashMap<Column<?>, Object>();
				for(Column<?> c : columnsToFactorOut)
					factoredOutValues.put(c, c.readValue(in));
			}
			
			//Read records:
			while(in.bitsAvailable() >= schema.getMinimumSize(columnsToFactorOut))
			{
				r = new Record(schema);
				r.readFromBitStream(in, columnsToFactorOut);
				//Set factored out values:
				if(columnsToFactorOut != null)
					for(Column<?> c : columnsToFactorOut)
						c.storeObject(r, factoredOutValues.get(c));
				records.add(r);
			}
		}
		catch(Exception e)
		{
			DecodeException de = new DecodeException("Error on decoding records.", e, schema, records); //pass schema used for decoding and records decoded so far 
			de.addRecord(r); //add last (partially decoded) record (will be ignored if null)
			records.clear(); //remove partially decoded records
			throw de;
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
	
	protected byte[] compress(byte[] data) throws IOException
	{
		byte[] compressedData = CompressorFactory.getCompressor(settings.getCompressionMode()).compress(data);
		compressionRatio = ((float) compressedData.length) / data.length;
		return compressedData;
	}
	
	protected byte[] decompress(byte[] compressedData) throws IOException
	{
		byte[] decompressedData = CompressorFactory.getCompressor(settings.getCompressionMode()).decompress(compressedData);
		compressionRatio = ((float) compressedData.length) / decompressedData.length;
		return decompressedData;
	}

	protected byte[] encrypt(byte[] data) throws IOException
	{
		//TODO encryption
		
		return data;
	}
	
	protected byte[] decrypt(byte[] data) throws IOException
	{
		//TODO decryption
		
		return data;
	}
	
	protected abstract void serialiseAndSplit(byte[] data) throws TransmissionCapacityExceededException;
	
	protected abstract byte[] mergeAndDeserialise() throws IOException;
	
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
		//TODO resent of individual part
	}
	
	/**
	 * Part has been sent
	 * 
	 * @param smsMessage
	 */
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
	
	/**
	 * Part has been delivered to relay
	 * 
	 * @param smsMessage
	 */
	public void partDelivered(Message smsMessage)
	{
		boolean allDelivered = true;
		DateTime lastDeliveredAt = null;
		for(Message m : parts)
		{
			if(!m.isReceived())
			{
				allDelivered = false;
				break;
			}
			else
			{
				if(lastDeliveredAt == null || lastDeliveredAt.isBefore(m.getDeliveredAt()))
					lastDeliveredAt = m.getDeliveredAt();
			}
		}
		if(allDelivered)
			deliveredAt = lastDeliveredAt;		
	}

	/**
	 * Part has been received by server (used only on server side, reception acknowledgements are per transmission)
	 * 
	 * @param smsMessage
	 */
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
	
	public float getCompressionRatio()
	{
		return compressionRatio;
	}
	
	public SMSAgent getReceiver()
	{
		return receiver;
	}
	
	public SMSAgent getSender()
	{
		return sender;
	}
	
}
