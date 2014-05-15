/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.sms;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.BinaryTransmission;
import uk.ac.ucl.excites.sapelli.transmission.DecodeException;
import uk.ac.ucl.excites.sapelli.transmission.IncompleteTransmissionException;
import uk.ac.ucl.excites.sapelli.transmission.Settings;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionSender;


/**
 * @author mstevens
 *
 */
public abstract class SMSTransmission extends BinaryTransmission
{
	
	protected SMSAgent receiver;
	protected SMSAgent sender;
	
	protected SortedSet<Message> parts;
	
	private DateTime deliveredAt;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param receiver
	 * @param settings
	 */
	public SMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, SMSAgent receiver, Settings settings)
	{
		super(schema, columnsToFactorOut, settings);
		this.receiver = receiver;
		this.parts = new TreeSet<Message>();
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param modelProvider
	 */
	public SMSTransmission(TransmissionClient modelProvider)
	{
		this(modelProvider, null);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param modelProvider
	 * @param parts
	 */
	public SMSTransmission(TransmissionClient modelProvider, List<Message> parts)
	{
		super(modelProvider);
		this.parts = new TreeSet<Message>();
		if(parts != null)
			for(Message m : parts)
				addPart(m);
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
			partReceived(); // to update receivedAt timestamp
		}
	}
	
	public SortedSet<Message> getParts()
	{
		return parts;
	}
	
	public Message getPart(int i)
	{
		for(Message part : parts)
			if(part.getPartNumber() == i)
				return part;
		return null;
	}
	
	public boolean hasPart(int i)
	{
		return getPart(i) != null;
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
	
	protected void sendPayload(TransmissionSender transmissionSender) throws Exception
	{
		if(parts.isEmpty())
			throw new IllegalStateException("No messages to send.");
		
		//Send unsent messages one by one:
		for(Message m : parts)
			if(!m.isSent())
				m.send(transmissionSender.getSMSService());
	}
	
	@Override
	protected void receivePayload(Schema schemaToUse, Settings settingsToUse) throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException
	{
		// First to a completeness check:
		if(!isComplete())
			throw new IncompleteTransmissionException(this);
		
		// Read messages:
		super.receivePayload(schemaToUse, settingsToUse);
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
			setSentAt(lastSentAt);
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
	private void partReceived()
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
			setReceivedAt(lastReceivedAt);
	}
	
	public SMSAgent getReceiver()
	{
		return receiver;
	}
	
	public SMSAgent getSender()
	{
		return sender;
	}
	
	public DateTime getDeliveredAt()
	{
		return deliveredAt;
	}
	
}
