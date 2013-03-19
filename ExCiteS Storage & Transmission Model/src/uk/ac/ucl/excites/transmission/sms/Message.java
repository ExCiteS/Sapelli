package uk.ac.ucl.excites.transmission.sms;

import java.util.Comparator;

import org.joda.time.DateTime;


/**
 * @author mstevens
 *
 */
public abstract class Message
{
	
	protected SMSAgent sender;
	protected SMSAgent receiver;
	protected SMSTransmission transmission;
	protected byte transmissionID;
	protected DateTime sentAt;
	protected DateTime receivedAt;
	protected int partNumber;
	protected int totalParts;
	

	/**
	 * To be called on sending side
	 * 
	 * @param receiver
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 */
	public Message(SMSAgent receiver, SMSTransmission transmission, int partNumber, int totalParts)
	{
		if(partNumber < 1 || totalParts < 1 || partNumber > totalParts)
			throw new IllegalArgumentException("Invalid part number (" + partNumber + ") of total number of parts (" + totalParts + ").");
		this.receiver = receiver;
		this.transmission = transmission;
		this.transmissionID = transmission.getID();
		this.partNumber  = partNumber;
		this.totalParts = totalParts;
	}
	
	/**
	 * To be called on receiver side
	 * 
	 * @param sender
	 */
	public Message(SMSAgent sender)
	{
		this.sender = sender;
	}
		
	protected abstract byte[] getPayload();
	
	public abstract void send();
	
	public void setTransmission(SMSTransmission transmission)
	{
		if(this.transmission == null)
			this.transmission = transmission;
		else
			throw new IllegalStateException("Cannot change transmission.");
	}	
	
	/**
	 * @return the transmissionID
	 */
	public byte getTransmissionID()
	{
		return transmissionID;
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
	
	public boolean isReceived()
	{
		return (receivedAt != null);
	}
	
	public DateTime getReceivedAt()
	{
		return receivedAt;
	}
	
	public void sentCallback()
	{
		sentAt = new DateTime(); //= now
		transmission.partSent(this);
	}

	public void receptionCallback()
	{
		receivedAt = new DateTime(); //TODO get actual time of reception by receiver?
		transmission.partReceived(this);
	}
	
	public SMSAgent getReceiver()
	{
		return receiver;
	}
	
	public SMSAgent getSender()
	{
		return sender;
	}
	
	public static class MessageComparator implements Comparator<Message>
	{

		@Override
		public int compare(Message lhs, Message rhs)
		{
			return lhs.getPartNumber() - rhs.getPartNumber();
		}
		
	}
	
}
