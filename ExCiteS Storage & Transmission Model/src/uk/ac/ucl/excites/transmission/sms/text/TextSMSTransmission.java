/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.text;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSService;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;

/**
 * @author mstevens
 *
 */
public class TextSMSTransmission extends SMSTransmission
{
	
	public TextSMSTransmission(Schema schema, byte id, SMSAgent receiver, SMSService smsService)
	{
		super(schema, new HashSet<Column<?>>(), id, receiver, smsService);
	}

	public TextSMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, byte id, SMSAgent receiver, SMSService smsService)
	{
		super(schema, columnsToFactorOut, id, receiver, smsService);
	}

	@Override
	protected List<Message> serialiseAndSplit(byte[] data) throws TransmissionCapacityExceededException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected byte[] mergeAndDeserialise(Set<Message> parts)
	{
		// TODO Auto-generated method stub
		return null;
	}

}
