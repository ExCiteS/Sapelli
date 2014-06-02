package uk.ac.ucl.excites.sapelli.transmission.sms.binary;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * A {@link Transmission} class which relies on series of up to 16 "binary" SMS messages, each represented by a {@link BinaryMessage}.
 * 
 * @author mstevens
 * 
 * @see BinaryMessage
 * @see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
 */
public class BinarySMSTransmission extends SMSTransmission<BinaryMessage>
{
	
	// Static
	public static final int MAX_TRANSMISSION_PARTS = 16;
	public static final int MAX_PAYLOAD_SIZE = MAX_TRANSMISSION_PARTS * BinaryMessage.MAX_PAYLOAD_SIZE_BYTES;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param receiver
	 */
	public BinarySMSTransmission(SMSAgent receiver)
	{
		super(receiver);
	}

	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 */
	public BinarySMSTransmission(TransmissionClient client)
	{
		super(client);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param parts
	 *
	 */
	public BinarySMSTransmission(TransmissionClient client, List<BinaryMessage> parts)
	{
		super(client, parts);
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
			parts.add(new BinaryMessage(receiver, this, parts.size() + 1, numberOfParts, partData));
			b += BinaryMessage.MAX_PAYLOAD_SIZE_BYTES;
		}
	}

	@Override
	protected byte[] deserialise() throws IOException
	{
		ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
		for(BinaryMessage part : parts)
			rawOut.write(((BinaryMessage) part).getPayload());
		rawOut.flush();
		rawOut.close();
		return rawOut.toByteArray();
	}
	
	public int getMaxPayloadBytes()
	{
		return MAX_PAYLOAD_SIZE;
	}

}
