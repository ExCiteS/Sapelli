package uk.ac.ucl.excites.sapelli.transmission.modes.sms;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * Abstract class representing an SMS message which is one part of an {@link SMSTransmission}
 * 
 * @author mstevens
 */
public abstract class Message implements Comparable<Message>
{
	
	protected int sendingSideTransmissionID;
	protected SMSTransmission<?> transmission;
	protected int payloadHash;
	
	protected TimeStamp sentAt;		 //only on sending side
	protected TimeStamp deliveredAt; //only on sending side
	protected TimeStamp receivedAt;	 //only on receiving side
	
	protected SMSAgent sender;	
	
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
	public Message(SMSTransmission<?> transmission, int partNumber, int totalParts)
	{
		// Some checks:
		if(!transmission.isLocalIDSet())
			throw new IllegalStateException("Cannot create Messages for sending until local transmission ID is set by means of persistent storage.");
		if(partNumber < 1 || totalParts < 1 || partNumber > totalParts)
			throw new IllegalArgumentException("Invalid part number (" + partNumber + ") of total number of parts (" + totalParts + ").");
		
		// Transmission:
		this.transmission = transmission;
		this.sendingSideTransmissionID = transmission.getLocalID();
		this.payloadHash = transmission.getPayloadHash();
		
		// Part numbers:
		this.partNumber  = partNumber;
		this.totalParts = totalParts;
	}
	
	/**
	 * To be called on receiving side
	 * 
	 * @param sender
	 */
	public Message(SMSAgent sender, TimeStamp receivedAt)
	{
		this.sender = sender;
		this.receivedAt = receivedAt;
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 * @param sentAt - may be null
	 * @param deliverdAt - may be null
	 * @param receivedAt - may be null
	 */
	protected Message(SMSTransmission<?> transmission, int partNumber, int totalParts, TimeStamp sentAt, TimeStamp deliverdAt, TimeStamp receivedAt)
	{
		this(transmission, partNumber, totalParts);
		this.sentAt = sentAt;
		this.deliveredAt = deliverdAt;
		this.receivedAt = receivedAt;
	}
	
	public abstract void send(SMSClient smsService);
	
	protected void setTransmission(SMSTransmission<?> transmission)
	{
		if(this.transmission != null && this.transmission != transmission)
			throw new IllegalStateException("Cannot change transmission.");
		this.transmission = transmission;
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
	
	public TimeStamp getSentAt()
	{
		return sentAt;
	}
	
	public void sentCallback()
	{
		sentAt = new TimeStamp(); //= now
		transmission.partSent(this);
	}
	
	public boolean isReceived()
	{
		return (receivedAt != null);
	}
	
	public TimeStamp getReceivedAt()
	{
		return receivedAt;
	}
	
	public boolean isDelivered()
	{
		return (deliveredAt != null);
	}
	
	public TimeStamp getDeliveredAt()
	{
		return deliveredAt;
	}

	public void deliveryCallback()
	{
		deliveredAt = new TimeStamp();
		transmission.partDelivered(this);
	}
	
	public SMSAgent getSender()
	{
		return sender;
	}
	
	/**
	 * @return the sendingSideTransmissionID
	 */
	public int getSendingSideTransmissionID()
	{
		return sendingSideTransmissionID;
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
	 * Deliberately ignores transmission (but not sendingSideTransmissionID), sentAt, deliveredAt & receivedAt.
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (sender == null ? 0 : sender.hashCode());
		hash = 31 * hash + sendingSideTransmissionID;
		hash = 31 * hash + payloadHash;
		hash = 31 * hash + partNumber;
		hash = 31 * hash + totalParts;
		hash = 31 * hash + getBodyHashCode();
		return hash;
	}
	
	/**
	 * equals() method
	 * Deliberately ignores transmission (but not sendingSideTransmissionID), sentAt, deliveredAt & receivedAt.
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
					this.sendingSideTransmissionID == another.sendingSideTransmissionID &&
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
