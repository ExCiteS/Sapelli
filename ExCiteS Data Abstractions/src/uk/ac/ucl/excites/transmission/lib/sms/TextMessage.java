/**
 * 
 */
package uk.ac.ucl.excites.transmission.lib.sms;

import uk.ac.ucl.excites.transmission.lib.model.simple.SMSAgent;
import uk.ac.ucl.excites.transmission.lib.model.simple.SMSTransmission;

/**
 * @author mstevens
 *
 */
public class TextMessage extends Message
{

	public TextMessage(SMSAgent receiver, SMSTransmission transmission)
	{
		super(receiver, transmission);

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
		// TODO Auto-generated method stub
		
	}

}
