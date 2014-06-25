/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.modes.sms;

import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.Sender;


/**
 * @author mstevens
 *
 */
public abstract class SMSTransmission<M extends Message> extends Transmission
{
	
	protected SMSAgent receiver;
	protected SMSAgent sender;
	
	protected final SortedSet<M> parts = new TreeSet<M>();
	
	private DateTime deliveredAt;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param receiver
	 * @param client
	 * @param payloadType
	 */
	public SMSTransmission(TransmissionClient client, SMSAgent receiver, Payload payload)
	{
		super(client, payload);
		this.receiver = receiver;
	}
		
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param sender
	 * @param firstReceivedPart
	 */
	public SMSTransmission(TransmissionClient client, SMSAgent sender, M firstReceivedPart)
	{
		super(client, firstReceivedPart.getPayloadHash());
		this.sender = sender;
		addPart(firstReceivedPart);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param localID
	 * @param sender may be null on sending side
	 * @param receiver may be null on receiving side
	 * @param payloadHash
	 * @param parts list of {@link Message}s
	 */
	public SMSTransmission(TransmissionClient client, int localID, String sender, String receiver, int payloadHash, List<M> parts) 
	{
		super(client, payloadHash);
		//TODO set seq id? (must come before setLocalID!)
		setLocalID(localID);
		
		//TODO this.sender = sender;
		//TODO this.receiver = receiver;
		if(parts != null)
			for(M m : parts)
				addPart(m);
	}
	
	/**
	 * To be called on receiving side.
	 * 
	 * @param msg
	 */
	public void addPart(M msg)
	{
		if(parts.isEmpty())
		{
			//TODO set seq id?
			this.sender = msg.getSender();
		}
		else
		{	//each following received message must have a matching transmission id, sender & partsTotal:
			if(payloadHash != msg.getPayloadHash())
				throw new IllegalArgumentException("This message does not belong to the same transmission (Payload hash mismatch)");
			//TODO seq id check?
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
	
	public SortedSet<M> getParts()
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
	@Override
	public boolean isComplete()
	{
		if(parts.isEmpty())
			return false;
		return (parts.first().getTotalParts() == parts.size());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Transmission#doSend(uk.ac.ucl.excites.sapelli.transmission.TransmissionSender)
	 */
	@Override
	protected void doSend(Sender transmissionSender)
	{
		if(parts.isEmpty())
			throw new IllegalStateException("No messages to send.");
		
		//Send unsent messages one by one:
		for(Message m : parts)
			if(!m.isSent())
				m.send(transmissionSender.getSMSService());
	}

	/**
	 * Resends a specific part (single message)
	 * 
	 * @param transmissionSender
	 * @param partNumber
	 */
	public void resend(Sender transmissionSender, int partNumber)
	{
		int i = 1; // partNumbers start from 1!
		for(Message m : parts)
			if(i == partNumber)
			{
				m.send(transmissionSender.getSMSService());
				return;
			}
			else
				i++;
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
	
	public boolean isReceiverSet()
	{
		return receiver != null;
	}
	
	public SMSAgent getSender()
	{
		return sender;
	}
	
	public boolean isSenderSet()
	{
		return sender != null;
	}
	
	public DateTime getDeliveredAt()
	{
		return deliveredAt;
	}
	
}
