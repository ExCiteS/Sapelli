/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.text;

import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.sms.SMSReceiver;
import uk.ac.ucl.excites.transmission.sms.SMSSender;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;

/**
 * @author mstevens
 *
 */
public class TextSMSTransmission extends SMSTransmission
{

	public TextSMSTransmission(Schema schema, Integer id, SMSReceiver receiver, SMSSender sender)
	{
		super(schema, id, receiver, sender);
	}

}
