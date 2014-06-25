package uk.ac.ucl.excites.sapelli.transmission.modes.sms;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * Abstract class representing an SMS message which is one part of an {@link SMSTransmission}
 * 
 * @author mstevens
 */
public abstract class Message implements Comparable<Message>
{
	
	protected int payloadHash;
	protected SMSTransmission<?> transmission;
	
	protected DateTime sentAt;		//only on sending side
	protected DateTime deliveredAt;	//only on sending side
	protected DateTime receivedAt;	//only on receiving side
	
	protected SMSAgent sender;
	protected SMSAgent receiver;	
	
	protected int partNumber;
	protected int totalParts;
	

	/**
	 * To be called on sending side.
	 * 
	 * @param receiver
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 */
	public Message(SMSAgent receiver, SMSTransmission<?> transmission, int partNumber, int totalParts)
	{
		if(partNumber < 1 || totalParts < 1 || partNumber > totalParts)
			throw new IllegalArgumentException("Invalid part number (" + partNumber + ") of total number of parts (" + totalParts + ").");
		this.receiver = receiver;
		this.payloadHash = transmission.getPayloadHash();
		this.transmission = transmission;
		this.partNumber  = partNumber;
		this.totalParts = totalParts;
	}
	
	/**
	 * To be called on receiving side
	 * 
	 * @param sender
	 */
	public Message(SMSAgent sender, DateTime receivedAt)
	{
		this.sender = sender;
		this.receivedAt = receivedAt;
	}
	
	public abstract void send(SMSClient smsService);
	
	public void setTransmission(SMSTransmission<?> transmission)
	{
		if(this.transmission == null)
			this.transmission = transmission;
		else
			throw new IllegalStateException("Cannot change transmission.");
	}
	
	/**
	 * @return the payloadHash
	 */
	public int getPayloadHash()
	{
		return payloadHash;
	}

	/**
	 * @return the partNumber
	 */
	public int getPartNumber()
	{
		return partNumber;
	}

	/**
	 * @return the totalParts
	 */
	public int getTotalParts()
	{
		return totalParts;
	}

	public boolean isSent()
	{
		return (sentAt != null);
	}
	
	public DateTime getSentAt()
	{
		return sentAt;
	}
	
	public void sentCallback()
	{
		sentAt = new DateTime(); //= now
		transmission.partSent(this);
	}
	
	public boolean isReceived()
	{
		return (receivedAt != null);
	}
	
	public DateTime getReceivedAt()
	{
		return receivedAt;
	}
	
	public boolean isDelivered()
	{
		return (deliveredAt != null);
	}
	
	public DateTime getDeliveredAt()
	{
		return deliveredAt;
	}

	public void deliveryCallback()
	{
		deliveredAt = new DateTime(); //TODO get actual time of reception by receiver?
		transmission.partDelivered(this);
	}
	
	public SMSAgent getReceiver()
	{
		return receiver;
	}
	
	public SMSAgent getSender()
	{
		return sender;
	}
	
	public SMSTransmission<?> getTransmission()
	{
		return transmission;
	}
		
	@Override
	public int compareTo(Message another)
	{
		return this.getPartNumber() - another.getPartNumber();
	}
	
	protected abstract int getBodyHashCode();
	
	/**
	 * hashCode() method
	 * Ignores transmission (but not transmissionID), sentAt, deliveredAt & receivedAt
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (sender == null ? 0 : sender.hashCode());
		hash = 31 * hash + (receiver == null ? 0 : receiver.hashCode());
		//TODO seq id
		hash = 31 * hash + payloadHash;
		hash = 31 * hash + partNumber;
		hash = 31 * hash + totalParts;
		hash = 31 * hash + getBodyHashCode();
		return hash;
	}
	
	/**
	 * equals() method
	 * Ignores transmission (but not transmissionID), sentAt, deliveredAt & receivedAt
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof Message)
		{
			Message another = (Message) obj;
			return	(this.sender == null ? another.sender == null : this.sender.equals(another.sender)) &&
					(this.receiver == null ? another.receiver == null : this.receiver.equals(another.receiver)) &&
					//TODO seq id
					this.payloadHash == another.payloadHash &&
					this.partNumber == another.partNumber &&
					this.totalParts == another.totalParts &&
					equalBody(another);			
		}
		return false;
	}
	
	protected abstract boolean equalBody(Message another);
	
	public abstract void setBody(TransmissionStore store, Record transmissionPartRecord);
	
}
