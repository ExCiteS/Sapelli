/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
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
	public static final int MAX_BODY_SIZE_BITS = MAX_TRANSMISSION_PARTS * BinaryMessage.MAX_BODY_SIZE_BITS;
	
	/**
	 * To be called on the sending side.
	 *
	 * @param client
	 * @param receiver
	 * @param payload
	 */
	public BinarySMSTransmission(TransmissionClient client, SMSCorrespondent receiver, Payload payload)
	{
		super(client, receiver, payload);
	}
		
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param firstReceivedPart
	 */
	public BinarySMSTransmission(TransmissionClient client, BinaryMessage firstReceivedPart)
	{
		super(client, firstReceivedPart);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param correspondent
	 * @param localID
	 * @param remoteID - may be null
	 * @param payloadHash
	 * @param sentAt - may be null
	 * @param receivedAt - may be null
	 * @param parts - list of {@link BinaryMessage}s
	 */	
	public BinarySMSTransmission(TransmissionClient client, SMSCorrespondent correspondent, int localID, Integer remoteID, int payloadHash, TimeStamp sentAt, TimeStamp receivedAt) 
	{
		super(client, correspondent, localID, remoteID, payloadHash, sentAt, receivedAt);
	}
	
	@Override
	protected void wrap(BitArray bodyBits) throws TransmissionCapacityExceededException, IOException
	{
		// Clear previously generated messages (!!!)
		parts.clear();
		
		// Capacity check:
		if(bodyBits.length() > MAX_BODY_SIZE_BITS)
			throw new TransmissionCapacityExceededException("Maximum body size (" + MAX_BODY_SIZE_BITS + " bits), exceeded by " + (bodyBits.length() - MAX_BODY_SIZE_BITS) + " bits");
		
		// Split up transmission bodyBits in parts (each becoming the body of a separate BinaryMessage):
		int numberOfParts = (bodyBits.length() + (BinaryMessage.MAX_BODY_SIZE_BITS - 1)) / BinaryMessage.MAX_BODY_SIZE_BITS;
		BitArrayInputStream stream = new BitArrayInputStream(bodyBits);
		for(int p = 0; p < numberOfParts; p++)
			parts.add(new BinaryMessage(this, p + 1, numberOfParts, stream.readBitArray(Math.min(BinaryMessage.MAX_BODY_SIZE_BITS, stream.bitsAvailable()))));		
		stream.close();
	}

	@Override
	protected BitArray unwrap() throws IOException
	{
		BitArrayOutputStream stream = new BitArrayOutputStream();
		for(BinaryMessage part : parts)
			stream.write(part.getBody());
		stream.flush();
		stream.close();
		return stream.toBitArray(); // return transmission body bits, possibly with some additional padding at the end (trailing 0s), this will be ignored in Transmission#receive()
	}

	@Override
	protected int getMaxBodyBits()
	{
		return MAX_BODY_SIZE_BITS;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Transmission#canWrapCanIncreaseSize()
	 */
	@Override
	public boolean canWrapIncreaseSize()
	{
		return false;
	}
	
	@Override
	public Type getType()
	{
		return Type.BINARY_SMS;
	}
	
	@Override
	public void handle(Handler handler)
	{
		handler.handle(this);
	}
	
}
