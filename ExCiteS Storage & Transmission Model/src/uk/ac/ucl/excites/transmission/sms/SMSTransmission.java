/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;

import java.util.ArrayList;
import java.util.List;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Transmission;


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
		super(schema);
		this.id = id;
		
		this.receiver = receiver;
		this.smsService = smsService;
		this.parts = new ArrayList<Message>();
	}
	
	@Override
	public boolean addRecord(Record record)
	{
		
		//BitArray dataToSend = new BitArray((HEADER_SIZE + content.length) * 8);
		//Construct header
		
		//Copy contents:
		//dataToSend.setBytes(HEADER_SIZE * 8, content);
		//Get byte[]
		//byte[] bytes = dataToSend.toBytes();

//		BitArray observationBits = observation.toBitArray(false);
//		
//		int headerSize = 12 * 8; //TODO move to statics
//		
//		BitArray newContent = new BitArray(headerSize + (content == null ? 0 : content.size()) + observationBits.size());
//		
//		//TODO Set header (may not yet?)
//		//TODO Copy old content
//		
//		content = newContent;
//		//TODO add observationBits to content
//		
//		//TODO Compress
//		
//		//TODO Encrypt
//		
//		//TODO SERIALISE/SPLIT
//		
//		//TODO check is everything fits
//		
//		//TODO Accept observation or reject it, if reject: mark as full
//		observations.add(observation);
//		observation.setTransaction(this);

		return true;
	}

	public boolean isFull()
	{
		return full;
	}

	public byte getID()
	{
		return id;
	}

	@Override
	public void send()
	{
		/* create and send sms messages */

		
	}

	public void resend(int part)
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
