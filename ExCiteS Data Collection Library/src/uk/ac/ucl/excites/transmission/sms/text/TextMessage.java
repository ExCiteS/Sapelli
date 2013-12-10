/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.text;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSService;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.util.BinaryHelpers;

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
	
	//Static
	public static final boolean MULTIPART = false; //we use standard single-part SMS messages, because using 
	public static final int MAX_TOTAL_CHARS = 160; // 	concatenated SMS would cause us to lose 7 chars per message
	public static final int HEADER_CHARS = 4; // = 28 bits
	public static final int MAX_PAYLOAD_CHARS = MAX_TOTAL_CHARS - HEADER_CHARS;
	
	private static IntegerRangeMapping PART_NUMBER_FIELD = new IntegerRangeMapping(1, TextSMSTransmission.MAX_TRANSMISSION_PARTS);
	private static final int HEADER_SEPARATOR_BIT = 1;

	//Dynamic
	private String payload;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param receiver
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 * @param payload
	 */
	public TextMessage(SMSAgent receiver, SMSTransmission transmission, int partNumber, int totalParts, String payload)
	{
		super(receiver, transmission, partNumber, totalParts);
		if(payload == null)
			throw new NullPointerException("Payload cannot be null!");
		if(payload.length() > MAX_PAYLOAD_CHARS)
			throw new IllegalArgumentException("Payload is too long (max: " + MAX_PAYLOAD_CHARS + " chars; got: " + payload.length() + " chars).");
		this.payload = payload;
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
		this(sender, text, new DateTime() /*received NOW*/);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param sender
	 * @param content	= header + payload
	 * @param receivedAt
	 * @throws Exception
	 */
	public TextMessage(SMSAgent sender, String content, DateTime receivedAt) throws Exception
	{
		super(sender, receivedAt);
		
		// Read header:
		long[] headerParts = new long[HEADER_CHARS];
		for(int h = 0; h < HEADER_CHARS; h++)
		{
			// Read header char:
			int c = TextSMSTransmission.GSM_0338_REVERSE_CHAR_TABLE.get(content.charAt(h));
			// Check separator bit:
			if(c >> (TextSMSTransmission.BITS_PER_CHAR - 1) != HEADER_SEPARATOR_BIT)
				throw new Exception("Invalid message (illegal header).");
			// Strip away the separator bit:
			headerParts[h] = c - (HEADER_SEPARATOR_BIT << (TextSMSTransmission.BITS_PER_CHAR - 1));
		}
		// Reassemble 24bit header value:
		int header = (int) BinaryHelpers.mergeLong(headerParts, TextSMSTransmission.BITS_PER_CHAR - 1);
		// Read header fields:
		transmissionID = (header >> (PART_NUMBER_FIELD.getSize() * 2));
		partNumber = (header >> PART_NUMBER_FIELD.getSize()) % (1 << PART_NUMBER_FIELD.getSize());
		totalParts = header % (1 << PART_NUMBER_FIELD.getSize());
		
		// Set payload:
		payload = content.substring(HEADER_CHARS);
	}

	public String getPayload()
	{
		return payload;
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
		// Merge header fields to 24 bit value:
		int header = (transmission.getID() << (PART_NUMBER_FIELD.getSize() * 2)) +	// ID (= CRC16 hash): takes up first 16 bits
					 (partNumber << (PART_NUMBER_FIELD.getSize())) +				// partNumber: takes up next 4 bits
					 totalParts;													// totalParts: takes up last 4 bits
		// Split header in 4 * 6 bit parts:
		long[] headerParts = BinaryHelpers.splitLong(header, HEADER_CHARS, (TextSMSTransmission.BITS_PER_CHAR - 1));
		// Insert a separator bit in front of each part and encode to a 7 bit character:
		for(int h = 0; h < HEADER_CHARS; h++)
		{	// Note: the chosen separator bit value (=1) ensures none of the header chars will ever be 'ESC'
			int c = (HEADER_SEPARATOR_BIT << TextSMSTransmission.BITS_PER_CHAR - 1) + (int) headerParts[h];
			if(c == TextSMSTransmission.ESCAPE_ESC)
				throw new IllegalStateException("Cannot encode 0x1B ('ESC')!"); // (this should never happen)
			// Add header char to content:
			blr.append(TextSMSTransmission.GSM_0338_CHAR_TABLE[c]);
		}
		
		//Write payload:
		blr.append(payload);
		
		return blr.toString();
	}

	@Override
	public void send(SMSService smsService)
	{
		smsService.send(this);
	}
	
	@Override
	protected int getPayloadHashCode()
	{
		return payload.hashCode();
	}
	
	public boolean isMultiPart()
	{
		return false;
	}

}
