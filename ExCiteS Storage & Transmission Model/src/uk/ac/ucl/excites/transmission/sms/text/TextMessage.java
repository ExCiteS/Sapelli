/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.text;

import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;

/**
 * @author mstevens
 *
 */
public class TextMessage extends Message
{

	private String content;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param receiver
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 * @param payload
	 */
	public TextMessage(SMSAgent receiver, SMSTransmission transmission, int partNumber, int totalParts, byte[] payload)
	{
		super(receiver, transmission, partNumber, totalParts);
		//TODO encode bytes in 7bit SMS text format
	}

	/**
	 * To be called on the receiving side.
	 * 
	 * @param sender
	 * @param text
	 */
	public TextMessage(SMSAgent sender, String text)
	{
		super(sender);
		this.content = text;
	}
	
	public String getText()
	{
		return content;
	}
	
	@Override
	public byte[] getPayload()
	{
		//TODO decode to bytes
		return new byte[140];
	}

	@Override
	public void send()
	{
		transmission.getSMSService().send(this);
	}

}
