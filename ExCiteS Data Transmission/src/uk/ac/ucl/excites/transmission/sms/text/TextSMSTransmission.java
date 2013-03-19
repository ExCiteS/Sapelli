/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.text;

import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSService;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;

/**
 * @author mstevens
 *
 */
public class TextSMSTransmission extends SMSTransmission
{

	public TextSMSTransmission(Schema schema, byte id, SMSAgent receiver, SMSService smsService)
	{
		super(schema, id, receiver, smsService);
	}

}
