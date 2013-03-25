package uk.ac.ucl.excites.transmission.sms.binary;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.SchemaProvider;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;
import uk.ac.ucl.excites.util.BinaryHelpers;

/**
 * @author mstevens
 *
 */
public class BinarySMSTransmission extends SMSTransmission
{
	
	public static final int MAX_TRANSMISSION_PARTS = 16;
	
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
	 * @param schemaProvider
	 * @param settings
	 */
	public BinarySMSTransmission(SchemaProvider schemaProvider, Settings settings)
	{
		super(schemaProvider, settings);
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
			byte[] partData = BinaryHelpers.subByteArray(data, b, BinaryMessage.MAX_PAYLOAD_SIZE_BYTES);
			Message msg = new BinaryMessage(receiver, this, messages.size() + 1, numberOfParts, partData);
			messages.add(msg);
			b += BinaryMessage.MAX_PAYLOAD_SIZE_BYTES;
		}
		return messages;
	}

	@Override
	protected byte[] mergeAndDeserialise(SortedSet<Message> parts) throws IOException
	{
		ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
		for(Message part : parts)
			rawOut.write(((BinaryMessage) part).getPayload());
		rawOut.flush();
		rawOut.close();
		return rawOut.toByteArray();
	}

}
