package uk.ac.ucl.excites.sapelli.transmission.sms.binary;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.Settings;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;
import uk.ac.ucl.excites.sapelli.util.BinaryHelpers;

/**
 * @author mstevens
 *
 */
public class BinarySMSTransmission extends SMSTransmission
{
	
	public static final int MAX_TRANSMISSION_PARTS = 16;
	public static final int MAX_PAYLOAD_SIZE = MAX_TRANSMISSION_PARTS * BinaryMessage.MAX_PAYLOAD_SIZE_BYTES;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param id
	 * @param receiver
	 * @param settings
	 */
	public BinarySMSTransmission(Schema schema, int id, SMSAgent receiver, Settings settings)
	{
		super(schema, null, id, receiver, settings);
	}
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param id
	 * @param receiver
	 * @param settings
	 */
	public BinarySMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, int id, SMSAgent receiver, Settings settings)
	{
		super(schema, columnsToFactorOut, id, receiver, settings);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param modelProvider
	 *
	 */
	public BinarySMSTransmission(TransmissionClient modelProvider)
	{
		super(modelProvider);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param modelProvider
	 * @param parts
	 *
	 */
	public BinarySMSTransmission(TransmissionClient modelProvider, List<Message> parts)
	{
		super(modelProvider, parts);
	}
	
	@Override
	protected void serialise(byte[] data) throws TransmissionCapacityExceededException
	{
		parts.clear();  //!!! clear previously generated messages
		if(data.length > MAX_PAYLOAD_SIZE)
			throw new TransmissionCapacityExceededException("Maximum payload size (" + MAX_PAYLOAD_SIZE + "), exceeded by " + (data.length - MAX_PAYLOAD_SIZE) + " bytes");
		int numberOfParts = (data.length / BinaryMessage.MAX_PAYLOAD_SIZE_BYTES) + ((data.length % BinaryMessage.MAX_PAYLOAD_SIZE_BYTES) > 0 ? 1 : 0);
		int b = 0;
		while(b < data.length)
		{
			byte[] partData = BinaryHelpers.subByteArray(data, b, BinaryMessage.MAX_PAYLOAD_SIZE_BYTES);
			Message msg = new BinaryMessage(receiver, this, parts.size() + 1, numberOfParts, partData);
			parts.add(msg);
			b += BinaryMessage.MAX_PAYLOAD_SIZE_BYTES;
		}
	}

	@Override
	protected byte[] deserialise() throws IOException
	{
		ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
		for(Message part : parts)
			rawOut.write(((BinaryMessage) part).getPayload());
		rawOut.flush();
		rawOut.close();
		return rawOut.toByteArray();
	}

}
