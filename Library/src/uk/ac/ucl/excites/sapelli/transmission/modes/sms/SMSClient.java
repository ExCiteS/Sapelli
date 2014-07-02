package uk.ac.ucl.excites.sapelli.transmission.modes.sms;

import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextMessage;

/**
 * @author mstevens
 *
 */
public interface SMSClient
{

	public boolean send(SMSAgent receiver, BinaryMessage binarySMS);
	
	public boolean send(SMSAgent receiver, TextMessage textSMS);
	
}
