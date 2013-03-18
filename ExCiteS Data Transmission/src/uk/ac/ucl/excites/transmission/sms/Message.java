package uk.ac.ucl.excites.transmission.sms;

import org.joda.time.DateTime;


/**
 * @author mstevens
 *
 */
public abstract class Message
{
	
	protected SMSSender sender;
	protected SMSReceiver receiver;
	protected SMSTransmission transmission;
	protected DateTime sentAt;
	protected DateTime receivedAt;
	

	public Message(SMSSender sender, SMSReceiver receiver, SMSTransmission transmission)
	{
		this.receiver = receiver;
		this.transmission = transmission;
	}
	
	public abstract int getMaxContentSize();
	
	public abstract void setContent(byte[] content);
	
	public abstract void send();
	
	public boolean isSent()
	{
		return (sentAt != null);
	}
	
	public boolean isReceived()
	{
		return (receivedAt != null);
	}
	
	public void sentCallback()
	{
		sentAt = new DateTime();
		transmission.partSent(this);
	}

	public void receptionCallback()
	{
		receivedAt = new DateTime(); //TODO get actual time of reception by receiver?
		transmission.partReceived(this);
	}	
	
}
