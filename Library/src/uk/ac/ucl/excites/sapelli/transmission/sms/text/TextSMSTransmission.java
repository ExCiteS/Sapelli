/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.sms.text;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.Settings;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * A {@link Transmission} class which relies on series of up to 16 plain "textual" SMS messages,
 * each represented by a {@link TextMessage}.
 * <br/><br/>
 * The binary data that is being sent is encoded to 7-bit characters from the default GSM 03.38 alphabet.
 * The encoding is done using 2 algorithms (both designed by Matthias Stevens):
 * <ul>
 * <li>one for the transmission part headers: see {@link TextMessage#getContent()} and {@link TextMessage#TextMessage(SMSAgent, String, org.joda.time.DateTime)};
 * <li>and another one for the transmission payload: see {@link #serialise(byte[])} and {@link #deserialise()}.
 * </ul>
 * The encoding algorithms are designed to avoid the reserved <code>ESC</code> character by means of escaping (thereby effectively avoiding that
 * escaping and use of the alphabet extension would be triggered on the lower Android level, which could cause messages to grow beyond part boundaries),
 * while keeping overhead to a strict minimum.
 * Header overhead is fixed at 4 bits (see {@link TextMessage}). Payload overhead is 1 bit per occurrence of a character that needs escaping (<code>SP</code> or <code>ESC</code>, i.e. 2 out of 128).
 * 
 * @author mstevens
 * 
 * @see TextMessage
 * @see <a href="http://en.wikipedia.org/wiki/Short_Message_Service">SMS</a>
 * @see <a href="http://en.wikipedia.org/wiki/GSM_03.38">GSM 03.38 / 3GPP TS 23.038</a>
 */
public class TextSMSTransmission extends SMSTransmission
{
	
	//Statics
	public static final int BITS_PER_CHAR = 7;
	public static final int MAX_TRANSMISSION_PARTS = 16;
	public static final int MAX_PAYLOAD_CHARS = MAX_TRANSMISSION_PARTS * TextMessage.MAX_PAYLOAD_CHARS;
	
	/*package*/ static final int ESCAPE_SP	= 0x20;
	/*package*/ static final int ESCAPE_ESC	= 0x1B;
	
	/**
	 * Basic Character Set of the GSM 7 bit default alphabet (standardised in GSM 03.38 / 3GPP TS 23.038)
	 * 
	 * @see <a href="http://en.wikipedia.org/wiki/GSM_03.38">GSM 03.38 / 3GPP TS 23.038</a> 
	 */
	public static final char[] GSM_0338_CHAR_TABLE =
	{
		/*	0	0x00	@	*/	'\u0040',
		/*	1	0x01	£	*/	'\u00A3',
		/*	2	0x02	$	*/	'\u0024',
		/*	3	0x03	¥	*/	'\u00A5',
		/*	4	0x04	è	*/	'\u00E8',
		/*	5	0x05	é	*/	'\u00E9',
		/*	6	0x06	ù	*/	'\u00F9',
		/*	7	0x07	ì	*/	'\u00EC',
		/*	8	0x08	ò	*/	'\u00F2',
		/*	9	0x09	Ç	*/	'\u00C7',
		/*	10	0x0A	LF	*/	'\n',
		/*	11	0x0B	Ø	*/	'\u00D8',
		/*	12	0x0C	ø	*/	'\u00F8',
		/*	13	0x0D	CR	*/	'\r',
		/*	14	0x0E	Å	*/	'\u00C5',
		/*	15	0x0F	å	*/	'\u00E5',
		/*	16	0x10	Δ	*/	'\u0394',
		/*	17	0x11	_	*/	'\u005F',
		/*	18	0x12	Φ	*/	'\u03A6',
		/*	19	0x13	Γ	*/	'\u0393',
		/*	20	0x14	Λ	*/	'\u039B',
		/*	21	0x15	Ω	*/	'\u03A9',
		/*	22	0x16	Π	*/	'\u03A0',
		/*	23	0x17	Ψ	*/	'\u03A8',
		/*	24	0x18	Σ	*/	'\u03A3',
		/*	25	0x19	Θ	*/	'\u0398',
		/*	26	0x1A	Ξ	*/	'\u039E',
		/*	27	0x1B	ESC	*/	'\u0020', //!!! ESCAPE encoded as SP (should never happen!)
		/*	28	0x1C	Æ	*/	'\u00C6',
		/*	29	0x1D	æ	*/	'\u00E6',
		/*	30	0x1E	ß	*/	'\u00DF',
		/*	31	0x1F	É	*/	'\u00C9',
		/*	32	0x20	SP	*/	'\u0020',
		/*	33	0x21	!	*/	'\u0021',
		/*	34	0x22	"	*/	'\u0022',
		/*	35	0x23	#	*/	'\u0023',
		/*	36	0x24	¤	*/	'\u00A4',
		/*	37	0x25	%	*/	'\u0025',
		/*	38	0x26	&	*/	'\u0026',
		/*	39	0x27	'	*/	'\'',
		/*	40	0x28	(	*/	'\u0028',
		/*	41	0x29	)	*/	'\u0029',
		/*	42	0x2A	*	*/	'\u002A',
		/*	43	0x2B	+	*/	'\u002B',
		/*	44	0x2C	,	*/	'\u002C',
		/*	45	0x2D	-	*/	'\u002D',
		/*	46	0x2E	.	*/	'\u002E',
		/*	47	0x2F	/	*/	'\u002F',
		/*	48	0x30	0	*/	'\u0030',
		/*	49	0x31	1	*/	'\u0031',
		/*	50	0x32	2	*/	'\u0032',
		/*	51	0x33	3	*/	'\u0033',
		/*	52	0x34	4	*/	'\u0034',
		/*	53	0x35	5	*/	'\u0035',
		/*	54	0x36	6	*/	'\u0036',
		/*	55	0x37	7	*/	'\u0037',
		/*	56	0x38	8	*/	'\u0038',
		/*	57	0x39	9	*/	'\u0039',
		/*	58	0x3A	:	*/	'\u003A',
		/*	59	0x3B	;	*/	'\u003B',
		/*	60	0x3C	<	*/	'\u003C',
		/*	61	0x3D	=	*/	'\u003D',
		/*	62	0x3E	>	*/	'\u003E',
		/*	63	0x3F	?	*/	'\u003F',
		/*	64	0x40	¡	*/	'\u00A1',
		/*	65	0x41	A	*/	'\u0041',
		/*	66	0x42	B	*/	'\u0042',
		/*	67	0x43	C	*/	'\u0043',
		/*	68	0x44	D	*/	'\u0044',
		/*	69	0x45	E	*/	'\u0045',
		/*	70	0x46	F	*/	'\u0046',
		/*	71	0x47	G	*/	'\u0047',
		/*	72	0x48	H	*/	'\u0048',
		/*	73	0x49	I	*/	'\u0049',
		/*	74	0x4A	J	*/	'\u004A',
		/*	75	0x4B	K	*/	'\u004B',
		/*	76	0x4C	L	*/	'\u004C',
		/*	77	0x4D	M	*/	'\u004D',
		/*	78	0x4E	N	*/	'\u004E',
		/*	79	0x4F	O	*/	'\u004F',
		/*	80	0x50	P	*/	'\u0050',
		/*	81	0x51	Q	*/	'\u0051',
		/*	82	0x52	R	*/	'\u0052',
		/*	83	0x53	S	*/	'\u0053',
		/*	84	0x54	T	*/	'\u0054',
		/*	85	0x55	U	*/	'\u0055',
		/*	86	0x56	V	*/	'\u0056',
		/*	87	0x57	W	*/	'\u0057',
		/*	88	0x58	X	*/	'\u0058',
		/*	89	0x59	Y	*/	'\u0059',
		/*	90	0x5A	Z	*/	'\u005A',
		/*	91	0x5B	Ä	*/	'\u00C4',
		/*	92	0x5C	Ö	*/	'\u00D6',
		/*	93	0x5D	Ñ	*/	'\u00D1',
		/*	94	0x5E	Ü	*/	'\u00DC',
		/*	95	0x5F	§	*/	'\u00A7',
		/*	96	0x60	¿	*/	'\u00BF',
		/*	97	0x61	a	*/	'\u0061',
		/*	98	0x62	b	*/	'\u0062',
		/*	99	0x63	c	*/	'\u0063',
		/*	100	0x64	d	*/	'\u0064',
		/*	101	0x65	e	*/	'\u0065',
		/*	102	0x66	f	*/	'\u0066',
		/*	103	0x67	g	*/	'\u0067',
		/*	104	0x68	h	*/	'\u0068',
		/*	105	0x69	i	*/	'\u0069',
		/*	106	0x6A	j	*/	'\u006A',
		/*	107	0x6B	k	*/	'\u006B',
		/*	108	0x6C	l	*/	'\u006C',
		/*	109	0x6D	m	*/	'\u006D',
		/*	110	0x6E	n	*/	'\u006E',
		/*	111	0x6F	o	*/	'\u006F',
		/*	112	0x70	p	*/	'\u0070',
		/*	113	0x71	q	*/	'\u0071',
		/*	114	0x72	r	*/	'\u0072',
		/*	115	0x73	s	*/	'\u0073',
		/*	116	0x74	t	*/	'\u0074',
		/*	117	0x75	u	*/	'\u0075',
		/*	118	0x76	v	*/	'\u0076',
		/*	119	0x77	w	*/	'\u0077',
		/*	120	0x78	x	*/	'\u0078',
		/*	121	0x79	y	*/	'\u0079',
		/*	122	0x7A	z	*/	'\u007A',
		/*	123	0x7B	ä	*/	'\u00E4',
		/*	124	0x7C	ö	*/	'\u00F6',
		/*	125	0x7D	ñ	*/	'\u00F1',
		/*	126	0x7E	ü	*/	'\u00FC',
		/*	127	0x7F	à	*/	'\u00E0'
	};

	static public HashMap<Character,Integer> GSM_0338_REVERSE_CHAR_TABLE = new HashMap<Character, Integer>(GSM_0338_CHAR_TABLE.length);
	static
	{
		for(int c = 0; c < GSM_0338_CHAR_TABLE.length; c++)
			if(c != ESCAPE_ESC)
				GSM_0338_REVERSE_CHAR_TABLE.put(GSM_0338_CHAR_TABLE[c], c);
	}
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param receiver
	 * @param settings
	 */
	public TextSMSTransmission(Schema schema, SMSAgent receiver, Settings settings)
	{
		super(schema, Collections.<Column<?>> emptySet(), receiver, settings);
	}

	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param receiver
	 * @param settings
	 */
	public TextSMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, SMSAgent receiver, Settings settings)
	{
		super(schema, columnsToFactorOut, receiver, settings);
	}

	/**
	 * To be called on the receiving side.
	 * 
	 * @param modelProvider
	 */
	public TextSMSTransmission(TransmissionClient modelProvider)
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
	public TextSMSTransmission(TransmissionClient modelProvider, List<Message> parts)
	{
		super(modelProvider, parts);
	}
	
	@Override
	protected void serialise(byte[] data) throws TransmissionCapacityExceededException, IOException
	{
		// Clear previously generated messages
		parts.clear(); //!!!
		
		// Convert data bytes to transmission payload String:
		String payload = null;
		BitInputStream bis = null;
		try
		{
			bis = new BitInputStream(new ByteArrayInputStream(data));
			Boolean escapeBit = null;
			StringBuilder bld = new StringBuilder();
			while(bis.bitsAvailable() + (escapeBit == null ? 0 : 1) > 0)
			{
				// Read 7, 6 or less bits and shift them to the right to fill 7 or 6 bits:
				int readBits = Math.min(bis.bitsAvailable(), BITS_PER_CHAR - (escapeBit == null ? 0 : 1));
				int c = (readBits > 0 ? (int) bis.readInteger(readBits, false) : 0) << (BITS_PER_CHAR - (escapeBit == null ? 0 : 1) - readBits);
				// Insert escape bit for previous character in most significant position (if needed):
				if(escapeBit != null)
					c += ((escapeBit ? 1 : 0) << (BITS_PER_CHAR - 1));
				// Escaping for current character: 
				if(c == ESCAPE_ESC || c == ESCAPE_SP)
				{	// Current character needs escaping:
					escapeBit = (c == ESCAPE_ESC); //1: ESC; 0: SP
					c = ESCAPE_SP; //we write SP instead of ESC and SP stays SP, the first bit of the next character will determine which of both it was
				}
				else
					escapeBit = null; //Current character doesn't need escaping			
				// Error handling:
				if(c == ESCAPE_ESC)
					throw new IllegalStateException("Cannot encode 0x1B ('ESC')!"); // (this should never happen)
				// Write character:
				bld.append(TextSMSTransmission.GSM_0338_CHAR_TABLE[c]);
			}
			payload = bld.toString();
		}
		finally
		{
			bis.close();
		}
		
		// Check length:
		if(payload.length() > MAX_PAYLOAD_CHARS)
			throw new TransmissionCapacityExceededException("Maximum payload size (" + MAX_PAYLOAD_CHARS + " characters), exceeded by " + (payload.length() - MAX_PAYLOAD_CHARS) + " characters");
		
		// Split up transmission payload in parts:
		String[] payloadParts = payload.split("(?<=\\G.{" + TextMessage.MAX_PAYLOAD_CHARS + "})");
		for(String payloadPart : payloadParts)
		{
			Message msg = new TextMessage(receiver, this, parts.size() + 1, payloadParts.length, payloadPart);
			parts.add(msg);
		}
	}

	@Override
	protected byte[] deserialise() throws IOException
	{
		// Assemble transmission payload String from part payload Strings:
		StringBuilder blr = new StringBuilder();
		for(Message part : parts)
			blr.append(((TextMessage) part).getPayload());
		String payloadString = blr.toString();

		// Convert transmission payload String to byte array:
		BitOutputStream bos = null;
		try
		{
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			bos = new BitOutputStream(baos);	
			boolean prevPrevSP = false;
			boolean prevSP = false;
			for(int i = 0; i < payloadString.length(); i++)
			{
				int c = TextSMSTransmission.GSM_0338_REVERSE_CHAR_TABLE.get(payloadString.charAt(i));
				boolean currSP = (c == ESCAPE_SP);
				if(!prevSP)
				{
					if(!currSP)
						bos.write(c, BITS_PER_CHAR, false); //write all 7 bits for current
				}
				else //prevSP = true
				{
					boolean escapeBit = ((c >> (BITS_PER_CHAR - 1)) == 1);
					bos.write((escapeBit ? ESCAPE_ESC : ESCAPE_SP) % (1 << (BITS_PER_CHAR - (prevPrevSP ? 1 : 0))), (BITS_PER_CHAR - (prevPrevSP ? 1 : 0)), false); //write 7 or 6 bits for previous
					if(!currSP)
						bos.write(c % (1 << (BITS_PER_CHAR - 1)), BITS_PER_CHAR - 1, false); //write 6 remaining bits for current
				}
				prevPrevSP = prevSP;
				prevSP = currSP;
			}
			//Do *not* call bos.flush(), we only want whole bytes!
			return baos.toByteArray();
		}
		finally
		{
			if(bos != null)
				bos.close();
		}
	}
	
}
