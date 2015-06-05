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

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * A {@link Transmission} class which relies on series of up to {@value MAX_TRANSMISSION_PARTS} plain "textual" SMS messages,
 * each represented by a {@link TextMessage}.
 * <br/><br/>
 * The binary data that is being sent is encoded to 7-bit characters from the default GSM 03.38 alphabet (see {@link Encoding#GSM_0338_CHAR_TABLE}).<br/>
 * The encoding is performed using 2 algorithms (both designed by Matthias Stevens):
 * <ul>
 * <li>one for the message header: see {@link TextMessage}, {@link TextMessage#getContent()} and {@link TextMessage#TextMessage(SMSCorrespondent, String, TimeStamp)}</li>
 * <li>and another one for the body: see {@link #wrap(BitArray)} and {@link #unwrap()}.</li>
 * </ul>
 * Both encoding algorithms are designed to avoid ever producing the reserved {@code ESC} character, which plays a role in the SMS alphabet extension mechanism.
 * Avoidance of this character is achieved by different strategies for the header and body:
 * <ul>
 * <li>	refer to {@link TextMessage} for an explanation of the strategy used for the message header;
 * <li> the strategy used for the message body is an escaping mechanism in which a 7 bit pattern that would normally map to {@code ESC} is instead encoded as {@code SP}. To differentiate which
 * 		{@code SP} occurrences are "real" ones and which are the result of {@code ESC} avoidance an additional bit is inserted (i.e. the first bit of the pattern represented by the character
 * 		which follows the {@code SP} occurrence), indicating how an {@code SP} occurrence must be interpreted upon decoding.</li>
 * </ul>
 * Both strategies keep overhead to a strict minimum:
 * <ul>
 * <li>	Header overhead is fixed at 1 bit per 6 effective header bits (see {@link TextMessage}).</li>
 * <li> Body overhead is 1 additional bit for every occurrence in the payload data of the 7 bit "{@code ESC} pattern" or the 7 bit "{@code SP} pattern" (i.e. 2 out of 128).</li>
 * </ul>
 * 
 * @author mstevens
 * 
 * @see TextMessage
 * @see Encoding
 * @see Decoding
 * @see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
 * @see <a href="http://en.wikipedia.org/wiki/GSM_03.38">GSM 03.38 / 3GPP TS 23.038</a>
 */
public class TextSMSTransmission extends SMSTransmission<TextMessage>
{
	
	// STATIC -------------------------------------------------------
	public static final int BITS_PER_CHAR = 7;
	public static final int MAX_TRANSMISSION_PARTS = 16;
	public static final int MAX_BODY_CHARS = MAX_TRANSMISSION_PARTS * TextMessage.MAX_BODY_CHARS;
	
	// DYNAMIC ------------------------------------------------------
	/**
	 * To be called on the sending side.
	 * 
	 * @param client
	 * @param receiver
	 * @param payload
	 */
	public TextSMSTransmission(TransmissionClient client, SMSCorrespondent receiver, Payload payload)
	{
		super(client, receiver, payload);
	}
		
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param firstReceivedPart
	 */
	public TextSMSTransmission(TransmissionClient client, TextMessage firstReceivedPart)
	{
		super(client, firstReceivedPart);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param correspondent
	 * @param received
	 * @param localID
	 * @param remoteID - may be null
	 * @param payloadType - may be null
	 * @param payloadHash
	 * @param sentAt - may be null
	 * @param receivedAt - may be null
	 * @param parts - list of {@link TextMessage}s
	 * @param numberOfSentResentRequests
	 * @param lastResendReqSentAt - may be null
	 */
	public TextSMSTransmission(TransmissionClient client, SMSCorrespondent correspondent, boolean received, int localID, Integer remoteID, Integer payloadType, int payloadHash, TimeStamp sentAt, TimeStamp receivedAt, int numberOfSentResentRequests, TimeStamp lastResendReqSentAt) 
	{
		super(client, correspondent, received, localID, remoteID, payloadType, payloadHash, sentAt, receivedAt, numberOfSentResentRequests, lastResendReqSentAt);
	}
	
	private int minNumberOfCharactersNeededFor(int bits)
	{
		return (bits + BITS_PER_CHAR - 1) / BITS_PER_CHAR;
	}
	
	@Override
	protected void wrap(BitArray bodyBits) throws TransmissionCapacityExceededException, IOException
	{
		// Clear previously generated messages
		parts.clear(); //!!!
		
		// Rough body length check (does not taking escaping into account, hence the "at least"):
		if(minNumberOfCharactersNeededFor(bodyBits.length()) > MAX_BODY_CHARS)
			throw new TransmissionCapacityExceededException("Maximum body size (" + MAX_BODY_CHARS + " characters) exceeded by at least " + minNumberOfCharactersNeededFor(bodyBits.length()) + " characters");
		
		// Convert transmission body from BitArray to String:
		StringBuilder bld = new StringBuilder();
		BitInputStream bitsIn = new BitArrayInputStream(bodyBits);
		try
		{
			Boolean escapeBit = null;
			while(bitsIn.bitsAvailable() + (escapeBit == null ? 0 : 1) > 0)
			{
				// Check if there is room for one more character:
				if(bld.length() + 1 > MAX_BODY_CHARS)					
					throw new TransmissionCapacityExceededException("Maximum body size (" + MAX_BODY_CHARS + " characters) exceeded by at least " + minNumberOfCharactersNeededFor(bitsIn.bitsAvailable() + (escapeBit == null ? 0 : 1)) + " characters");
				// Read 7, 6 or less bits and shift them to the right to fill 7 or 6 bits:
				int readBits = Math.min(bitsIn.bitsAvailable(), BITS_PER_CHAR - (escapeBit == null ? 0 : 1));
				int c = (readBits > 0 ? (int) bitsIn.readInteger(readBits, false) : 0) << (BITS_PER_CHAR - (escapeBit == null ? 0 : 1) - readBits); // insert trailing 0s if less than 7 or 6 bits were read
				// Insert escape bit for previous character in most significant position (if needed):
				if(escapeBit != null)
					c += escapeBit ? (1 << (BITS_PER_CHAR - 1)) : 0;
				/* Escaping for current character:
				 * 	The escaping mechanism is only concerned with the characters ESC & SP.
				 * 	The only "illegal" character is ESC, to avoid it we replace it by SP.
				 * 	A normal occurrence of SP will stay SP. To differentiate both we insert
				 * 	an "escapeBit" as the first bit of the next character (shifting the real
				 * 	data bits over 1 position), the state of which indicates whether the
				 * 	preceding character was ESC (escapeBit = 1) or SP (escapeBit = 0). */
				switch(c)
				{
					case Encoding.ESCAPE_ESC :		// ESC:
						escapeBit = true;	//	escapeBit = 1 (true)
						c = Encoding.ESCAPE_SP;		//	write SP instead of ESC
						break;
					case Encoding.ESCAPE_SP :		// SP:
						escapeBit = false;	//	escapeBit = 0 (false)
						break;
					default :
						escapeBit = null;	// character doesn't need escaping
				}
				// Write character:
				bld.append(Encoding.GSM_0338_CHAR_TABLE[c]);
			}
		}
		finally
		{
			bitsIn.close();
		}
			
		// Split up transmission body string in parts (each becoming the body of a separate TextMessage):
		String transmissionBodyStr = bld.toString();
		int partsTotal = (transmissionBodyStr.length() + TextMessage.MAX_BODY_CHARS - 1) / TextMessage.MAX_BODY_CHARS;
		for(int p = 0; p < partsTotal; p++)
			parts.add(new TextMessage(this, p + 1, partsTotal, transmissionBodyStr.substring(p * TextMessage.MAX_BODY_CHARS, Math.min((p + 1) * TextMessage.MAX_BODY_CHARS, transmissionBodyStr.length()))));
	}

	@Override
	protected BitArray unwrap() throws IOException
	{
		// Assemble transmission body String from message body Strings:
		StringBuilder blr = new StringBuilder();
		for(TextMessage part : parts)
			blr.append(part.getBody());
		String transmissionBodyStr = blr.toString();

		// Convert transmission body from String to BitArray:
		BitArrayOutputStream bitsOut = new BitArrayOutputStream();
		try
		{
			boolean prevPrevSP = false;
			boolean prevSP = false;
			for(int i = 0, n = transmissionBodyStr.length(); i < n; i++)
			{
				int c = Decoding.GSM_0338_REVERSE_CHAR_TABLE.get(transmissionBodyStr.charAt(i));
				boolean currSP = (c == Encoding.ESCAPE_SP);
				if(!prevSP)
				{
					if(!currSP)
						bitsOut.write(c, BITS_PER_CHAR, false); // write all 7 bits for current
				}
				else // prevSP = true
				{
					boolean escapeBit = ((c >> (BITS_PER_CHAR - 1)) == 1);
					bitsOut.write((escapeBit ? Encoding.ESCAPE_ESC : Encoding.ESCAPE_SP) % (1 << (BITS_PER_CHAR - (prevPrevSP ? 1 : 0))), (BITS_PER_CHAR - (prevPrevSP ? 1 : 0)), false); // write 7 or 6 bits for previous
					if(!currSP)
						bitsOut.write(c % (1 << (BITS_PER_CHAR - 1)), BITS_PER_CHAR - 1, false); // write 6 remaining bits for current
				}
				prevPrevSP = prevSP;
				prevSP = currSP;
			}
			bitsOut.close();
			return bitsOut.toBitArray(); // return transmission body bits, possibly with some additional padding at the end (trailing 0s), this will be ignored in Transmission#receive()
		}
		finally
		{
			bitsOut.close();
		}
	}
	
	protected int getMaxBodyBits()
	{
		return MAX_BODY_CHARS * BITS_PER_CHAR;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Transmission#canWrapCanIncreaseSize()
	 */
	@Override
	public boolean canWrapIncreaseSize()
	{
		return true; // due to escaping mechanism
	}
	
	@Override
	public Type getType()
	{
		return Type.TEXTUAL_SMS;
	}
	
	@Override
	public void handle(Handler handler)
	{
		handler.handle(this);
	}
	
}
