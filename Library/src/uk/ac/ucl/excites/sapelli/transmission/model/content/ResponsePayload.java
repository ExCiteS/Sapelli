package uk.ac.ucl.excites.sapelli.transmission.model.content;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * A "response" payload is one that is sent as a result of receiving some other transmission, and thus includes information about the "subject" transmission (e.g. an ACK).
 * 
 * @author benelliott
 *
 */
public abstract class ResponsePayload extends Payload
{
	
	protected int subjectSenderSideID;
	protected int subjectPayloadHash;
	
	public ResponsePayload()
	{
		super();
	}
	
	public ResponsePayload(Transmission<?> subject)
	{
		this.subjectSenderSideID = subject.getRemoteID();
		this.subjectPayloadHash = subject.getPayloadHash();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		Transmission.TRANSMISSION_ID_FIELD.write(subjectSenderSideID, bitstream);
		Transmission.PAYLOAD_HASH_FIELD.write(subjectPayloadHash, bitstream);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		subjectSenderSideID = Transmission.TRANSMISSION_ID_FIELD.readInt(bitstream);
		subjectPayloadHash = Transmission.PAYLOAD_HASH_FIELD.readInt(bitstream);
	}
	
	/**
	 * @return the subjectSenderSideID
	 */
	public int getSubjectSenderSideID()
	{
		return subjectSenderSideID;
	}

	/**
	 * @return the subjectPayloadHash
	 */
	public int getSubjectPayloadHash()
	{
		return subjectPayloadHash;
	}
}
