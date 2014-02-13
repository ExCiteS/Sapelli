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
 * A {@link Transmission} class which relies on series of up to 16 "binary" SMS messages, each represented by a {@link BinaryMessage}.
 * 
 * @author mstevens
 * 
 * @see BinaryMessage
 * @see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
 */
public class BinarySMSTransmission extends SMSTransmission
{
	
	// Static
	public static final int MAX_TRANSMISSION_PARTS = 16;
	public static final int MAX_PAYLOAD_SIZE = MAX_TRANSMISSION_PARTS * BinaryMessage.MAX_PAYLOAD_SIZE_BYTES;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param receiver
	 * @param settings
	 */
	public BinarySMSTransmission(Schema schema, SMSAgent receiver, Settings settings)
	{
		super(schema, null, receiver, settings);
	}
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param receiver
	 * @param settings
	 */
	public BinarySMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, SMSAgent receiver, Settings settings)
	{
		super(schema, columnsToFactorOut, receiver, settings);
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
			throw new TransmissionCapacityExceededException("Maximum payload size (" + MAX_PAYLOAD_SIZE + " bytes), exceeded by " + (data.length - MAX_PAYLOAD_SIZE) + " bytes");
		int numberOfParts = (data.length + (BinaryMessage.MAX_PAYLOAD_SIZE_BYTES - 1)) / BinaryMessage.MAX_PAYLOAD_SIZE_BYTES;
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
