package uk.ac.ucl.excites.sapelli.transmission.sms;

import uk.ac.ucl.excites.sapelli.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.sms.text.TextMessage;

/**
 * @author mstevens
 *
 */
public interface SMSService
{

	public boolean send(BinaryMessage binarySMS);
	
	public boolean send(TextMessage textSMS);
	
}
