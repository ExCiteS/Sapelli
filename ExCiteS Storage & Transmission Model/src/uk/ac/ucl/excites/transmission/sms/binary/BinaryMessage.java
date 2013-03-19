/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.binary;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.util.BinaryHelpers;
import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;

/**
 * @author mstevens
 *
 */
public class BinaryMessage extends Message
{

	//Static
	public static final int MAX_TOTAL_SIZE_BYTES = 133; //in Bytes (Android takes 7 bits for the header)
	public static final int MAX_TRANSMISSION_PARTS = 16;
	private static IntegerRangeMapping PART_NUMBER_FIELD = new IntegerRangeMapping(1, MAX_TOTAL_SIZE_BYTES);
	public static final int HEADER_SIZE_BITS =	SMSTransmission.ID_LENGTH_BITS /* Transmission ID */ +
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
	 */
	public BinaryMessage(SMSAgent receiver, SMSTransmission transmission, int partNumber, int totalParts, byte[] payload)
	{
		super(receiver, transmission, partNumber, totalParts);
		if(totalParts > MAX_TRANSMISSION_PARTS)
			throw new IllegalArgumentException("Max transmission length exceded (" + totalParts + "; max is " + MAX_TRANSMISSION_PARTS + ").");
		if(payload.length > MAX_PAYLOAD_SIZE_BYTES)
			throw new IllegalArgumentException("Content is too long (max size: " + MAX_PAYLOAD_SIZE_BYTES + ")");
		this.payload = payload;
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param sender
	 * @param data
	 * @throws Exception
	 */
	public BinaryMessage(SMSAgent sender, byte[] data) throws Exception
	{
		super(sender);
		//read data:
		BitInputStream in = null;
		try
		{
			//Input stream:
			in = new BitInputStream(new ByteArrayInputStream(data));
			
			//Read header:
			transmissionID = in.readByte();					//Transmission ID
			partNumber = (int) PART_NUMBER_FIELD.read(in);	//Part number
			totalParts = (int) PART_NUMBER_FIELD.read(in);	//Total parts
			
			//Read payload:
			payload = in.readBytes(data.length - BinaryHelpers.bytesNeeded(HEADER_SIZE_BITS), true);
		}
		catch(Exception e)
		{
			throw new Exception("Error upon reading message data.", e);
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

	@Override
	protected byte[] getPayload()
	{
		return payload;
	}
	
	/**
	 * Called by sender
	 * 
	 * @return
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
			out.write(transmission.getID());			//Transmission ID
			PART_NUMBER_FIELD.write(partNumber, out);	//Part number
			PART_NUMBER_FIELD.write(totalParts, out);	//Total parts
			
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
	public void send()
	{
		transmission.getSMSService().send(this);
	}

}
