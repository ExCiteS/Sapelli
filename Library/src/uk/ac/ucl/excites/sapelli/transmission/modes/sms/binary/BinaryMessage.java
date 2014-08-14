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

package uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;

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
	 * 	_without_ requiring an UDH (User Data Header) to be inserted in the body this type of functionality does not seem to be exposed in Android.
	 *  Instead Android only allows sending "data messages" by means of the "Application port addressing scheme", which requires an UDH to be
	 *  inserted in the body. This scheme exists in 2 varieties, one using 8-bit port numbers (UDH IEI = 04), the other using 16-bit port numbers (UDH IEI = 05).
	 * 	In the former case the UDH takes up 5 bytes, in the later 7 bytes. However it is unclear whether Android will actually use a 5 byte header if a small
	 * 	enough (i.e. needing <= 8 bit) port number is specified, or whether it always uses the 7 byte header.<br/>
	 *  <br/>
	 *  For now we will assume a 7 byte UDH is (or could be) used and therefore limit the usable message contents to 133 (= 140 - 7) bytes.
	 * 	
	 * 	@see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
	 *  @see <a href="http://en.wikipedia.org/wiki/User_Data_Header">User Data Header (UDH)</a>
	 *  @see <a href="http://en.wikipedia.org/wiki/GSM_03.40">GSM 03.40 / 3GPP TS 23.040</a>
	 *  @see <a href="http://en.wikipedia.org/wiki/GSM_03.38#GSM_8_bit_data_encoding">GSM 03.38 / 3GPP TS 23.038: 8-bit data encoding</a>
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
	 * @param partNumber
	 * @param totalParts
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
	public BinaryMessage(SMSAgent sender, byte[] data) throws Exception
	{
		this(sender, data, TimeStamp.now() /*received NOW*/);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param sender
	 * @param data
	 * @param receivedAt
	 * @throws Exception
	 */
	public BinaryMessage(SMSAgent sender, byte[] data, TimeStamp receivedAt) throws Exception
	{
		super(sender, receivedAt);
		// Read data:
		BitInputStream in = null;
		try
		{
			//Input stream:
			in = new BitWrapInputStream(new ByteArrayInputStream(data));
			
			//Read header:
			sendingSideTransmissionID = (int) Transmission.TRANSMISSION_ID_FIELD.read(in);	// transmission ID on sending side
			payloadHash = (int) Transmission.PAYLOAD_HASH_FIELD.read(in);					// Payload hash
			partNumber = (int) PART_NUMBER_FIELD.read(in);									// Part number
			totalParts = (int) PART_NUMBER_FIELD.read(in);									// Total parts
			
			//Read payload:
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
	 * @param deliverdAt - may be null
	 * @param receivedAt - may be null
	 * @param body
	 */
	protected BinaryMessage(BinarySMSTransmission transmission, int partNumber, int totalParts, TimeStamp sentAt, TimeStamp deliverdAt, TimeStamp receivedAt, BitArray body)
	{
		super(transmission, partNumber, totalParts, sentAt, deliverdAt, receivedAt);
		this.body = body;
	}
	
	/**
	 * Called by {@link BinarySMSTransmission#unwrap()}
	 * 
	 * @return
	 */
	protected BitArray getBody()
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
	public void send(SMSClient smsService)
	{
		smsService.send(transmission.getReceiver(), this);
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
	public void setBody(TransmissionStore store, Record transmissionPartRecord)
	{
		store.setPartBody(body, transmissionPartRecord);
	}

}
