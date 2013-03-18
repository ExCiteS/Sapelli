/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;


/**
 * @author mstevens
 *
 */
public class TextMessage extends Message
{

	public TextMessage(SMSSender sender, SMSReceiver receiver, SMSTransmission transmission)
	{
		super(sender, receiver, transmission);

	}

	@Override
	public int getMaxContentSize()
	{

		return 0;
	}

	@Override
	public void setContent(byte[] content)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void send()
	{
		
		sender.send(this);
	}

}
