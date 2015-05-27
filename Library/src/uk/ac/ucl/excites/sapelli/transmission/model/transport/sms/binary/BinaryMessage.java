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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission.Type;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.InvalidMessageException;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.protocol.sms.SMSClient;

/**
 * Binary SMS message
 * 
 * @author mstevens
 * 
 * @see BinarySMSTransmission
 * @see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
 */
public class BinaryMessage extends Message
{

	// STATICS-------------------------------------------------------
	/**
	 * Our {@link BinaryMessage}s can contain up to 133 bytes of data.
	 * <br/>
	 * <b>Explanation:</b><br/>
	 * 	The body (called <code>TP_UD</code>) of an SMS message can contain up to 160 7-bit characters or the equivalent 140 bytes (octets)
	 * 	Even though the GSM/3GPP standards actually support the sending of "binary" SMSs (i.e. containing 8-bit bytes instead of 7-bit characters)
	 * 	_without_ requiring an UDH (User Data Header) to be inserted in the body, this type of functionality does not seem to be exposed in Android.
	 *  Instead Android only allows sending "data messages" by means of the "Application port addressing scheme", which requires an UDH to be
	 *  inserted in the body (taking up space that cannot be used to send actual user data). This scheme exists in 2 varieties, one using 8-bit port
	 *  numbers (UDH IEI = 04), the other using 16-bit port numbers (UDH IEI = 05). In the former case the UDH takes up 5 bytes, in the later 7 bytes.<br/>
	 *  <br/>
	 *  Sadly however, current Android versions default to using 16-bit port numbers even if the actual port number(s) would fit in 8-bits. This means
	 *  the UDH is always 7 bytes (and not 5) meaning there is only 133 bytes left for user data (and not 135).<br/>
	 *  By studying the Android source I found that support for 8-bit port addressing (and 5-byte UDH) is essentially there, but it is not used because
	 *  no size check on the destination port number is done. I've reported this problem to the Android developer mailing list (see link below).<br/>
	 *  <br/>
	 *  For now we have no choice but to always assume a 7 byte UDH is used and therefore we limit the usable message contents to 133 (= 140 - 7) bytes.
	 *  
	 * 	@see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
	 *  @see <a href="http://en.wikipedia.org/wiki/User_Data_Header">User Data Header (UDH)</a>
	 *  @see <a href="http://en.wikipedia.org/wiki/GSM_03.40">GSM 03.40 / 3GPP TS 23.040</a>
	 *  @see <a href="http://en.wikipedia.org/wiki/GSM_03.38#GSM_8_bit_data_encoding">GSM 03.38 / 3GPP TS 23.038: 8-bit data encoding</a>
	 *  @see <a href="https://code.google.com/p/android/issues/detail?id=75047">Bug report about lack of support for 8-bit port addressing</a>
	 */
	public static final int MAX_TOTAL_SIZE_BYTES = 133; //in bytes
	
	private static IntegerRangeMapping PART_NUMBER_FIELD = new IntegerRangeMapping(1, BinarySMSTransmission.MAX_TRANSMISSION_PARTS);
	
	public static final int HEADER_SIZE_BITS =	Transmission.TRANSMISSION_ID_FIELD.size() +		// Transmission ID
												Transmission.PAYLOAD_HASH_FIELD.size() +		// Payload hash
												PART_NUMBER_FIELD.size() +						// Part number
												PART_NUMBER_FIELD.size();						// Parts total
	
	public static final int MAX_BODY_SIZE_BITS = (MAX_TOTAL_SIZE_BYTES * Byte.SIZE) - HEADER_SIZE_BITS;
	
	// DYNAMICS------------------------------------------------------
	/**
	 * the body of the message, i.e. a part of the whole transmission body.
	 */
	private BitArray body;
	
	/**
	 * To be called on the sending side.
	 * Called by {@link BinarySMSTransmission#wrap(BitArray)}.
	 * 
	 * @param transmission
	 * @param partNumber a value from [1, totalParts]
	 * @param totalParts a value from [1, BinarySMSTransmission.MAX_TRANSMISSION_PARTS]
	 * @param payload
	 */
	protected BinaryMessage(BinarySMSTransmission transmission, int partNumber, int totalParts, BitArray body)
	{
		super(transmission, partNumber, totalParts);
		if(totalParts > BinarySMSTransmission.MAX_TRANSMISSION_PARTS)
			throw new IllegalArgumentException("Max transmission length exceded (" + totalParts + "; max is " + BinarySMSTransmission.MAX_TRANSMISSION_PARTS + ").");
		if(body.length() > MAX_BODY_SIZE_BITS)
			throw new IllegalArgumentException("Message body is too long (max size: " + MAX_BODY_SIZE_BITS + ")");
		this.body = body;
	}
	
	/**
	 * To be called on the receiving side (msg received *now*)
	 * 
	 * @param sender
	 * @param data
	 * @throws Exception
	 */
	public BinaryMessage(SMSCorrespondent sender, byte[] data) throws Exception
	{
		this(sender, data, TimeStamp.now() /*received NOW*/);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param sender
	 * @param data
	 * @param receivedAt
	 * @throws InvalidMessageException
	 * @throws Exception
	 */
	public BinaryMessage(SMSCorrespondent sender, byte[] data, TimeStamp receivedAt) throws InvalidMessageException, Exception
	{
		super(sender, receivedAt);
		// Read data:
		BitInputStream in = null;
		try
		{
			// Input stream:
			in = new BitWrapInputStream(new ByteArrayInputStream(data));
			
			// Check data size:
			if(data.length * Byte.SIZE < HEADER_SIZE_BITS)
				throw new InvalidMessageException("Data byte array is too short for this to be a valid Sapelli binary SMS message");
			
			// Read header:
			sendingSideTransmissionID = Transmission.TRANSMISSION_ID_FIELD.readInt(in);	// Transmission ID on sending side
			payloadHash = Transmission.PAYLOAD_HASH_FIELD.readInt(in);					// Payload hash
			partNumber = PART_NUMBER_FIELD.readInt(in);									// Part number
			totalParts = PART_NUMBER_FIELD.readInt(in);									// Total parts
			
			// Part number check:
			if(partNumber > totalParts)
				throw new InvalidMessageException("Inconsistent part number (" + partNumber + "/" + totalParts + "), this is not a valid Sapelli binary SMS message");
			
			// Read payload:
			body = in.readBitArray(in.bitsAvailable()); // may include trailing 0 padding if this is the last message in the transmission
		}
		catch(IOException ioe)
		{
			throw new Exception("Error upon reading message data.", ioe);
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(Exception ignore) {}
		}
	}

	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 * @param sentAt - may be null
	 * @param deliveredAt - may be null
	 * @param receivedAt - may be null
	 * @param body
	 */
	public BinaryMessage(BinarySMSTransmission transmission, int partNumber, int totalParts, TimeStamp sentAt, TimeStamp deliveredAt, TimeStamp receivedAt, BitArray body)
	{
		super(transmission, partNumber, totalParts, sentAt, deliveredAt, receivedAt);
		this.body = body;
	}
	
	/**
	 * Called by {@link BinarySMSTransmission#unwrap()} and {@link TransmissionStore}
	 * 
	 * @return
	 */
	public BitArray getBody()
	{
		return body;
	}
	
	/**
	 * Called by SMSClient
	 * 
	 * @return the full message content (= header + payload)
	 * @throws Exception
	 */
	public byte[] getBytes() throws Exception
	{
		byte[] data = null;
		BitOutputStream out = null;
		try
		{
			//Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitWrapOutputStream(rawOut);
	
			// Write header:
			Transmission.TRANSMISSION_ID_FIELD.write(sendingSideTransmissionID, out);	// Transmission ID on sending side
			Transmission.PAYLOAD_HASH_FIELD.write(payloadHash, out);					// Payload hash
			PART_NUMBER_FIELD.write(partNumber, out);									// Part number
			PART_NUMBER_FIELD.write(totalParts, out);									// Total parts
			
			// Write body:
			out.write(body);
			
			// Flush, close & get bytes:
			out.flush(); // will use padding (insertion of trailing 0s) if not at Byte boundary (only happens on last message in transmission)
			out.close();
	
			data = rawOut.toByteArray();
		}
		catch(Exception e)
		{
			throw new Exception("Error on assembling bytes of BinaryMessage.", e);
		}
		finally
		{
			try
			{
				if(out != null)
					out.close();
			}
			catch(Exception ignore) {}
		}
		if(data == null)
			throw new NullPointerException("Error on assembling bytes of BinaryMessage, byte array is null"); //should not happen
		if(data.length > MAX_TOTAL_SIZE_BYTES)
			throw new Exception("Error on assembling bytes of BinaryMessage, maximum length exceeded (" + data.length + ", max is " + MAX_TOTAL_SIZE_BYTES + ").");		
		return data;
	}

	@Override
	protected void doSend(SMSClient smsClient)
	{
		smsClient.send(transmission.getCorrespondent(), this);
	}

	@Override
	protected int getBodyHashCode()
	{
		return body.hashCode();
	}

	@Override
	protected boolean equalBody(Message another)
	{
		return another instanceof BinaryMessage && body.equals(((BinaryMessage) another).body);
	}
	
	@Override
	public void handle(Handler handler)
	{
		handler.handle(this);
	}

	@Override
	public Type getTransmissionType()
	{
		return Type.BINARY_SMS;
	}

}
