package uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary;

import java.io.IOException;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSTransmission;
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
	public static final int MAX_PAYLOAD_SIZE_BITS = MAX_TRANSMISSION_PARTS * BinaryMessage.MAX_BODY_SIZE_BITS;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param receiver
	 * @param client
	 * @param payloadType
	 */
	public BinarySMSTransmission(TransmissionClient client, SMSAgent receiver, Payload payload)
	{
		super(client, receiver, payload);
	}
		
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param sender
	 * @param firstReceivedPart
	 */
	public BinarySMSTransmission(TransmissionClient client, SMSAgent sender, BinaryMessage firstReceivedPart)
	{
		super(client, sender, firstReceivedPart);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param localID
	 * @param sender may be null on sending side
	 * @param receiver may be null on receiving side
	 * @param payloadHash
	 * @param parts list of {@link Message}s
	 */
	public BinarySMSTransmission(TransmissionClient client, int localID, String sender, String receiver, int payloadHash, List<BinaryMessage> parts) 
	{
		super(client, localID, sender, receiver, payloadHash, parts);
	}
	
	@Override
	protected void wrap(BitArray payloadBits) throws TransmissionCapacityExceededException, IOException
	{
		parts.clear();  //!!! clear previously generated messages
		if(payloadBits.length() > MAX_PAYLOAD_SIZE_BITS)
			throw new TransmissionCapacityExceededException("Maximum payload size (" + MAX_PAYLOAD_SIZE_BITS + " bits), exceeded by " + (payloadBits.length() - MAX_PAYLOAD_SIZE_BITS) + " bits");
		int numberOfParts = (payloadBits.length() + (BinaryMessage.MAX_BODY_SIZE_BITS - 1)) / BinaryMessage.MAX_BODY_SIZE_BITS;
		// Create parts:
		BitArrayInputStream stream = new BitArrayInputStream(payloadBits);
		for(int p = 0; p < numberOfParts; p++)
			parts.add(new BinaryMessage(receiver, this, p + 1, numberOfParts, stream.readBitArray(Math.min(BinaryMessage.MAX_BODY_SIZE_BITS, stream.bitsAvailable()))));		
		stream.close();
	}

	@Override
	protected BitArray unwrap() throws IOException
	{
		BitArrayOutputStream stream = new BitArrayOutputStream();
		for(BinaryMessage part : parts)
			stream.write(((BinaryMessage) part).getBody());
		stream.flush();
		stream.close();
		return stream.toBitArray();
	}

	@Override
	public int getMaxPayloadBits()
	{
		return MAX_PAYLOAD_SIZE_BITS;
	}

}
