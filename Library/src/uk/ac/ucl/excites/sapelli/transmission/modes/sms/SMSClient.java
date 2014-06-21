package uk.ac.ucl.excites.sapelli.transmission.modes.sms;

import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextMessage;

/**
 * @author mstevens
 *
 */
public interface SMSClient
{

	public boolean send(BinaryMessage binarySMS);
	
	public boolean send(TextMessage textSMS);
	
}
