/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.payloads;

import java.io.IOException;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * @author mstevens
 *
 */
public class AckPayload extends Payload
{

	private int subjectPayloadHash;
	private DateTime subjectReceivedAt;
	
	/**
	 * To be called from receiving side
	 * 
	 */
	public AckPayload()
	{
		super();
	}

	/**
	 * To be called from sending side
	 * 
	 */
	public AckPayload(Transmission receivedTransmission)
	{
		//TODO seq id
		this.subjectPayloadHash = receivedTransmission.getPayloadHash();
		this.subjectReceivedAt = receivedTransmission.getReceivedAt();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Payload#getType()
	 */
	@Override
	public int getType()
	{
		return BuiltinType.Ack.ordinal();
	}

	@Override
	protected void write(BitOutputStream bitsteam) throws IOException, TransmissionCapacityExceededException
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException
	{
		// TODO Auto-generated method stub
		
	}

	/**
	 * @return the subjectPayloadHash
	 */
	public int getSubjectPayloadHash()
	{
		return subjectPayloadHash;
	}

	/**
	 * @return the subjectReceivedAt
	 */
	public DateTime getSubjectReceivedAt()
	{
		return subjectReceivedAt;
	}

}
