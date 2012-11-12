package uk.ac.ucl.excites.transmission.lib.sms;

import java.sql.Timestamp;

import uk.ac.ucl.excites.transmission.lib.model.simple.SMSAgent;
import uk.ac.ucl.excites.transmission.lib.model.simple.SMSTransmission;

/**
 * @author mstevens
 *
 */
public abstract class Message
{
	
	protected SMSAgent receiver;
	protected SMSTransmission transmission;
	protected Timestamp sentAt;
	protected Timestamp receivedAt;
	

	public Message(SMSAgent receiver, SMSTransmission transmission)
	{
		this.receiver = receiver;
		this.transmission = transmission;
	}
	
	public abstract int getMaxContentSize();
	
	public abstract void setContent(byte[] content);
	
	public abstract void send();
	
	public void sentCallback()
	{
		sentAt = new Timestamp(System.currentTimeMillis());
		//TODO inform transmission
	}
	
	public boolean isSent()
	{
		return (sentAt != null);
	}
	
	public boolean isReceived()
	{
		return (receivedAt != null);
	}

	public void receptionCallback()
	{
		receivedAt = new Timestamp(System.currentTimeMillis()); //TODO get the actual time it was received by the receiver?
		transmission.partReceived(this);
	}
	
	
}
