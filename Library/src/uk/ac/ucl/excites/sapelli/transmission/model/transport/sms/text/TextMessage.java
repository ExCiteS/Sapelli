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

package uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text;

import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
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
 * Textual SMS message in which data is encoded as 7-bit characters using the default GSM 03.38 alphabet.
 * <br/><br/>
 * The contents of the message always start with a {@value #HEADER_SIZE_CHARS} character header, in which
 * we only use characters from a subset containing half of the GSM 03.38 alphabet. Hence only 6 out of 7
 * of the bits represented by each header character have actual meaning. This mechanism achieves 3 things:
 * <ul>
 * <li>it causes the header to take up exactly {@value #HEADER_SIZE_CHARS} characters, meaning the message
 * header can be easily separated from the message payload (because they are aligned to the character boundary);</li>
 * <li>the alphabet subset does not contain the <code>ESC</code> character, thereby avoiding the need for
 * an escaping mechanism (unlike for the transmission payload, see {@link TextSMSTransmission}), which thus
 * allows for a fixed header length;</li>
 * <li>the characters which <i>are</i> in the alphabet subset have been selected to reduce the risk of
 * human-written SMS messages being mistaken for Sapelli messages (see {@link Encoding#HEADER_CHAR_INDEX_MAPPING}).</li> 
 * </ul>
 * 
 * @author mstevens
 * 
 * @see TextSMSTransmission
 * @see Encoding
 * @see Decoding
 * @see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
 * @see <a href="http://en.wikipedia.org/wiki/GSM_03.38">GSM 03.38 / 3GPP TS 23.038</a>
 */
public class TextMessage extends Message
{
	
	// STATICS-------------------------------------------------------
	public static final boolean MULTIPART = false; // we use standard single-part SMS messages, because using concatenated SMS would cause us to lose 7 chars per message (i.e. 153 instead of 160 chars) 
	public static final int MAX_TOTAL_CHARS = 160;
	
	private static IntegerRangeMapping PART_NUMBER_FIELD = new IntegerRangeMapping(1, TextSMSTransmission.MAX_TRANSMISSION_PARTS);
	
	public static final int BITS_PER_HEADER_CHAR = TextSMSTransmission.BITS_PER_CHAR - 1;
	
	private static final int HEADER_SIZE_CHARS = (	Transmission.TRANSMISSION_ID_FIELD.size() +
													Transmission.PAYLOAD_HASH_FIELD.size() +
													PART_NUMBER_FIELD.size() +
													PART_NUMBER_FIELD.size()) / (BITS_PER_HEADER_CHAR); // = 8 chars
	
	public static final int MAX_BODY_CHARS = MAX_TOTAL_CHARS - HEADER_SIZE_CHARS;	

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
	 * @param partNumber a value from [1, totalParts]
	 * @param totalParts a value from [1, TextSMSTransmission.MAX_TRANSMISSION_PARTS]
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
	public TextMessage(SMSCorrespondent sender, String text) throws Exception
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
	public TextMessage(SMSCorrespondent sender, String content, TimeStamp receivedAt) throws Exception
	{
		super(sender, receivedAt);
		
		// Check content size:
		if(content.length() < HEADER_SIZE_CHARS)
			throw new InvalidMessageException("Message content is too short.");
		
		// Check content against alphabet (if it contains characters outside the basic GSM_0338 alphabet it is definitely not a Sapelli message):
		for(int h = 0; h < HEADER_SIZE_CHARS; h++)
			if(Decoding.GSM_0338_REVERSE_CHAR_TABLE.get(content.charAt(h)) == null)
				throw new InvalidMessageException("Message content contains invalid characters.");
		
		// Read header:
		//	Convert from chars to 6-bit integers:
		BitArrayOutputStream hdrFieldBitsOut = new BitArrayOutputStream();
		try
		{
			for(int h = 0; h < HEADER_SIZE_CHARS; h++)
				// Each header character represents 6 bits of meaningful information:
				hdrFieldBitsOut.write(Decoding.HEADER_VALUE_REVERSE_MAPPING.get(content.charAt(h)), BITS_PER_HEADER_CHAR, false); // get() will throw NPE if char not found
		}
		catch(NullPointerException npe)
		{
			throw new InvalidMessageException("Message content contains invalid characters in header.");
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
		
		// Part number check:
		if(partNumber > totalParts)
			throw new InvalidMessageException("Inconsistent part number (" + partNumber + "/" + totalParts + "), this is not a valid Sapelli text SMS message");
		
		// Set body:
		body = content.substring(HEADER_SIZE_CHARS);
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
		//	Split in groups of 6 bits and convert to chars:
		BitArrayInputStream hdrFieldBitsIn = new BitArrayInputStream(hdrFieldBitsOut.toBitArray());
		try
		{
			while(hdrFieldBitsIn.bitsAvailable() >= BITS_PER_HEADER_CHAR)
			{
				// Map 6-bit int to an index of a character from the GSM_0338 alphabet which can be used in the header:
				int c = Encoding.HEADER_CHAR_INDEX_MAPPING[(int) hdrFieldBitsIn.readInteger(BITS_PER_HEADER_CHAR, false)];	
				/*if(c == Encoding.ESCAPE_ESC)
					throw new IllegalStateException("Cannot encode 0x1B (ESC)!"); // (this should never happen)*/
				// Add header char to content:
				blr.append(Encoding.GSM_0338_CHAR_TABLE[c]);
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
	
	@Override
	public Type getTransmissionType()
	{
		return Type.TEXTUAL_SMS;
	}
	
}
