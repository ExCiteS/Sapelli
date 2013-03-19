package uk.ac.ucl.excites.transmission.sms.binary;

import java.util.ArrayList;
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
public class BinarySMSTransmission extends SMSTransmission
{
	
	public static final int MAX_TRANSMISSION_PARTS = 16;
	
	public BinarySMSTransmission(Schema schema, byte id, SMSAgent receiver, SMSService smsService)
	{
		super(schema, new HashSet<Column<?>>(), id, receiver, smsService);
	}
	
	public BinarySMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, byte id, SMSAgent receiver, SMSService smsService)
	{
		super(schema, columnsToFactorOut, id, receiver, smsService);
	}

	@Override
	protected List<Message> serialiseAndSplit(byte[] data) throws TransmissionCapacityExceededException
	{
		int maxPayloadSize = MAX_TRANSMISSION_PARTS * BinaryMessage.MAX_PAYLOAD_SIZE_BYTES;
		if(data.length > maxPayloadSize)
			throw new TransmissionCapacityExceededException("MaxPayloadSize (" + maxPayloadSize + "), exceeded by " + (data.length - maxPayloadSize) + " bytes");
		int numberOfParts = (data.length / BinaryMessage.MAX_PAYLOAD_SIZE_BYTES) + ((data.length % BinaryMessage.MAX_PAYLOAD_SIZE_BYTES) > 0 ? 1 : 0);
		List<Message> messages = new ArrayList<Message>();
		int b = 0;
		while(b < data.length)
		{
			//TODO
			
		}
		return messages;
	}

	@Override
	protected byte[] mergeAndDeserialise(Set<Message> parts)
	{
		// TODO Auto-generated method stub
		return null;
	}

}
