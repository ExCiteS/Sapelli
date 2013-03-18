/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.binary;

import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSReceiver;
import uk.ac.ucl.excites.transmission.sms.SMSSender;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;

/**
 * @author mstevens
 *
 */
public class BinaryMessage extends Message
{

	//Static
	public static final int MAX_TOTAL_SIZE = 133; //in Bytes (Android takes 7 bits for the header)
	public static final int HEADER_SIZE = 2; //TODO determine actual needed header size
	public static final int MAX_CONTENT_SIZE = MAX_TOTAL_SIZE - HEADER_SIZE;
	
	//Dynamic
	public byte[] content;
	
	public BinaryMessage(SMSSender sender, SMSReceiver receiver, SMSTransmission transmission)
	{
		super(sender, receiver, transmission);
	}
	
	@Override
	public int getMaxContentSize()
	{
		return MAX_CONTENT_SIZE;
	}

	@Override
	public void setContent(byte[] content)
	{
		if(content.length >= getMaxContentSize())
			throw new IllegalArgumentException("Content is too long (max size: " + getMaxContentSize() + ")");
		this.content = content;
	}
	
	public byte[] getContent()
	{
		return content;
	}

	@Override
	public void send()
	{
		sender.send(this);
	}

}
