/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.text;

import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSReceiver;
import uk.ac.ucl.excites.transmission.sms.SMSSender;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;


/**
 * @author mstevens
 *
 */
public class TextMessage extends Message
{

	private String content;
	
	public TextMessage(SMSSender sender, SMSReceiver receiver, SMSTransmission transmission)
	{
		super(sender, receiver, transmission);
	}

	@Override
	public int getMaxContentSize()
	{
		return 0; //TODO 7*160 bits?
	}

	@Override
	public void setContent(byte[] content)
	{
		//TODO encode bytes in 7bit SMS text format
	}
	
	public String getContent()
	{
		return content;
	}

	@Override
	public void send()
	{
		sender.send(this);
	}

}
