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

package uk.ac.ucl.excites.sapelli.transmission.modes.sms.text;

import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;

/**
 * Textual SMS message in which data in encoding as 7-bit characters using the default GSM 03.38 alphabet.
 * 
 * The contents of the message always start with a {@value #HEADER_CHARS} character header. Only 24 of the 28 bits
 * of the header have actual meaning ({@value TODO} bits for the hash, and twice {@value TODO} bits for the partNumber
 * and totalParts), but they are split in groups of 6 bits and prefixed by a separator bit (= {@value #HEADER_SEPARATOR_BIT}).
 * This achieves 2 things:
 * <ul>
 * <li>it causes the header to take up exactly 4 characters (4 * 7  = 28),
 * meaning the message header can be easily separated from the message payload
 * (because they are aligned to the character boundary);</li>
 * <li>the choice of the separator bit ensures that none of the header characters
 * will ever be <code>ESC</code>, thereby avoiding the need for additional escaping
 * (unlike is necessary for the transmission payload, see {@link TextSMSTransmission}),
 * which thus allows for a fixed header length.</li> 
 * </ul>
 * 
 * @author mstevens
 * 
 * @see TextSMSTransmission
 * @see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
 * @see <a href="http://en.wikipedia.org/wiki/GSM_03.38">GSM 03.38 / 3GPP TS 23.038</a>
 */
public class TextMessage extends Message
{
	
	// STATICS-------------------------------------------------------
	public static final boolean MULTIPART = false; // we use standard single-part SMS messages, because using 
	public static final int MAX_TOTAL_CHARS = 160; // 	concatenated SMS would cause us to lose 7 chars per message
	
	private static final int HEADER_SEPARATOR_BIT = 1; // chosen such that the ESC character is never produced in the header
	
	private static IntegerRangeMapping PART_NUMBER_FIELD = new IntegerRangeMapping(1, TextSMSTransmission.MAX_TRANSMISSION_PARTS);
	
	private static final int HEADER_CHARS = (Transmission.TRANSMISSION_ID_FIELD.size() +
											Transmission.PAYLOAD_HASH_FIELD.size() +
											PART_NUMBER_FIELD.size() +
											PART_NUMBER_FIELD.size()) / (TextSMSTransmission.BITS_PER_CHAR - 1 /* for separator bit */); // = 8 chars
	
	public static final int MAX_BODY_CHARS = MAX_TOTAL_CHARS - HEADER_CHARS;	

	// DYNAMICS------------------------------------------------------
	/**
	 * the body of the message, i.e. a part of the whole transmission body.
	 */
	private String body;
	
	/**
	 * To be called on the sending side.
	 * Called by {@link TextSMSTransmission#wrap(uk.ac.ucl.excites.sapelli.shared.io.BitArray)}.
	 * 
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 * @param body
	 */
	protected TextMessage(TextSMSTransmission transmission, int partNumber, int totalParts, String body)
	{
		super(transmission, partNumber, totalParts);
		if(body == null)
			throw new NullPointerException("Payload cannot be null!");
		if(body.length() > MAX_BODY_CHARS)
			throw new IllegalArgumentException("Payload is too long (max: " + MAX_BODY_CHARS + " chars; got: " + body.length() + " chars).");
		this.body = body;
	}

	/**
	 * To be called on the receiving side (msg received *now*)
	 * 
	 * @param sender
	 * @param data
	 * @throws Exception
	 */
	public TextMessage(SMSAgent sender, String text) throws Exception
	{
		this(sender, text, TimeStamp.now() /*received NOW*/);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param sender
	 * @param content	= header + payload
	 * @param receivedAt
	 * @throws Exception
	 */
	public TextMessage(SMSAgent sender, String content, TimeStamp receivedAt) throws Exception
	{
		super(sender, receivedAt);
		
		// Read header:
		//	Convert from chars and remove separator bits:
		BitArrayOutputStream hdrFieldBitsOut = new BitArrayOutputStream();
		try
		{
			for(int h = 0; h < HEADER_CHARS; h++)
			{
				// Read header char (7 bits):				
				int c = TextSMSTransmission.GSM_0338_REVERSE_CHAR_TABLE.get(content.charAt(h));
				// Check separator bit:
				if(c >> (TextSMSTransmission.BITS_PER_CHAR - 1) != HEADER_SEPARATOR_BIT)
					throw new Exception("Invalid message (illegal header).");
				// Strip away the separator bit and write remaining 6 bit header part:
				hdrFieldBitsOut.write(c - (HEADER_SEPARATOR_BIT << (TextSMSTransmission.BITS_PER_CHAR - 1)), TextSMSTransmission.BITS_PER_CHAR - 1, false); 
			}
		}
		finally
		{
			hdrFieldBitsOut.close();
		}
		//	Read header fields:
		BitArrayInputStream hdrFieldBitsIn = new BitArrayInputStream(hdrFieldBitsOut.toBitArray());
		sendingSideTransmissionID = Transmission.TRANSMISSION_ID_FIELD.readInt(hdrFieldBitsIn);
		payloadHash = Transmission.PAYLOAD_HASH_FIELD.readInt(hdrFieldBitsIn);
		partNumber = PART_NUMBER_FIELD.readInt(hdrFieldBitsIn);
		totalParts = PART_NUMBER_FIELD.readInt(hdrFieldBitsIn);
		hdrFieldBitsIn.close();
		
		// Set body:
		body = content.substring(HEADER_CHARS);
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
	public TextMessage(TextSMSTransmission transmission, int partNumber, int totalParts, TimeStamp sentAt, TimeStamp deliverdAt, TimeStamp receivedAt, String body)
	{
		super(transmission, partNumber, totalParts, sentAt, deliverdAt, receivedAt);
		this.body = body;
	}

	/**
	 * Called by {@link TextSMSTransmission#unwrap()} and {@link TransmissionStore}
	 * 
	 * @return
	 */
	public String getBody()
	{
		return body;
	}
	
	/**
	 * Called by sender
	 * 
	 * The header encoding is designed to avoid that the reserved {@code ESC} character is ever produced in the header characters.
	 * The strategy is to insert an additional "separator bit" before every group of 6 header bits, this bit is chosen such that the
	 * none of the resulting 7 bit patterns will ever map to the {@code ESC} character.
	 * 
	 * @return the full message content (= header + payload)
	 * @throws Exception
	 */
	public String getContent() throws Exception
	{
		StringBuilder blr = new StringBuilder();
		
		//Write header:
		//	Write header fields:
		BitArrayOutputStream hdrFieldBitsOut = new BitArrayOutputStream();
		Transmission.TRANSMISSION_ID_FIELD.write(sendingSideTransmissionID, hdrFieldBitsOut);	// Sending-side localID: takes up the first 24 bits
		Transmission.PAYLOAD_HASH_FIELD.write(payloadHash, hdrFieldBitsOut);					// Payload hash (= CRC16 hash): takes up the next 16 bits
		PART_NUMBER_FIELD.write(partNumber, hdrFieldBitsOut);									// partNumber: takes up next 4 bits
		PART_NUMBER_FIELD.write(totalParts, hdrFieldBitsOut);									// totalParts: takes up last 4 bits
		hdrFieldBitsOut.close();
		//	Insert separator bits and convert to chars:
		BitArrayInputStream hdrFieldBitsIn = new BitArrayInputStream(hdrFieldBitsOut.toBitArray());
		try
		{
			while(hdrFieldBitsIn.bitsAvailable() >= TextSMSTransmission.BITS_PER_CHAR - 1)
			{
				int c = (HEADER_SEPARATOR_BIT << TextSMSTransmission.BITS_PER_CHAR - 1) + (int) hdrFieldBitsIn.readInteger(TextSMSTransmission.BITS_PER_CHAR - 1, false);; // the separator bit ensures none of the header chars will ever be 'ESC'
				if(c == TextSMSTransmission.ESCAPE_ESC)
					throw new IllegalStateException("Cannot encode 0x1B (ESC)!"); // (this should never happen)
				// Add header char to content:
				blr.append(TextSMSTransmission.GSM_0338_CHAR_TABLE[c]);
			}
		}
		finally
		{
			hdrFieldBitsIn.close();
		}
		
		//Write body:
		blr.append(body);
		
		return blr.toString();
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
		return another instanceof TextMessage && body.equals(((TextMessage) another).body);
	}
	
	public boolean isMultiPart()
	{
		return false;
	}

	@Override
	public void handle(Handler handler)
	{
		handler.handle(this);
	}

}
