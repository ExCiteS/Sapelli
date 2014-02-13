package uk.ac.ucl.excites.transmission.sms;

import uk.ac.ucl.excites.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.transmission.sms.text.TextMessage;

/**
 * @author mstevens
 *
 */
public interface SMSService
{

	public boolean send(BinaryMessage binarySMS);
	
	public boolean send(TextMessage textSMS);
	
}
