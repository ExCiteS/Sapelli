package uk.ac.ucl.excites.transmission.sms.binary;

import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSService;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;

/**
 * @author mstevens
 *
 */
public class BinarySMSTransmission extends SMSTransmission
{
	
	public BinarySMSTransmission(Schema schema, byte id, SMSAgent receiver, SMSService smsService)
	{
		super(schema, id, receiver, smsService);
	}

}
