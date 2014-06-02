/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.sms.binary;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;
import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.transmission.BinaryTransmission;
import uk.ac.ucl.excites.sapelli.transmission.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSService;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSTransmission;

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

	//Static
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
	
	public static final int HEADER_SIZE_BITS =	BinaryTransmission.TRANSMISSION_ID_FIELD.getSize() /* Transmission ID */ +
												PART_NUMBER_FIELD.getSize() /* Part number */ +
												PART_NUMBER_FIELD.getSize() /* Parts total */; 
	
	public static final int MAX_PAYLOAD_SIZE_BYTES = MAX_TOTAL_SIZE_BYTES - BinaryHelpers.bytesNeeded(HEADER_SIZE_BITS);
	
	//Dynamic
	private byte[] payload;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param receiver
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 * @param payload
	 */
	public BinaryMessage(SMSAgent receiver, SMSTransmission<?> transmission, int partNumber, int totalParts, byte[] payload)
	{
		super(receiver, transmission, partNumber, totalParts);
		if(totalParts > BinarySMSTransmission.MAX_TRANSMISSION_PARTS)
			throw new IllegalArgumentException("Max transmission length exceded (" + totalParts + "; max is " + BinarySMSTransmission.MAX_TRANSMISSION_PARTS + ").");
		if(payload.length > MAX_PAYLOAD_SIZE_BYTES)
			throw new IllegalArgumentException("Content is too long (max size: " + MAX_PAYLOAD_SIZE_BYTES + ")");
		this.payload = payload;
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
		this(sender, data, new DateTime() /*received NOW*/);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param sender
	 * @param data
	 * @param receivedAt
	 * @throws Exception
	 */
	public BinaryMessage(SMSAgent sender, byte[] data, DateTime receivedAt) throws Exception
	{
		super(sender, receivedAt);
		//read data:
		BitInputStream in = null;
		try
		{
			//Input stream:
			in = new BitInputStream(new ByteArrayInputStream(data));
			
			//Read header:
			transmissionID = (int) BinaryTransmission.TRANSMISSION_ID_FIELD.read(in);	//Transmission ID
			partNumber = (int) PART_NUMBER_FIELD.read(in);				//Part number
			totalParts = (int) PART_NUMBER_FIELD.read(in);				//Total parts
			
			//Read payload:
			payload = in.readBytes(data.length - BinaryHelpers.bytesNeeded(HEADER_SIZE_BITS));
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
	 * Called by receiver
	 * 
	 * @return
	 */
	public byte[] getPayload()
	{
		return payload;
	}
	
	/**
	 * Called by sender
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
			out = new BitOutputStream(rawOut);
	
			//Write header:
			BinaryTransmission.TRANSMISSION_ID_FIELD.write(transmission.getID(), out);	//Transmission ID
			PART_NUMBER_FIELD.write(partNumber, out);				//Part number
			PART_NUMBER_FIELD.write(totalParts, out);				//Total parts
			
			//Write payload:
			out.write(payload);
			
			// Flush, close & get bytes:
			out.flush();
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
	public void send(SMSService smsService)
	{
		smsService.send(this);
	}

	@Override
	protected int getPayloadHashCode()
	{
		return Arrays.hashCode(payload);
	}

}
