package uk.ac.ucl.excites.sapelli.transmission.model.content;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextSMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Payload class for resend requests to be used when the receiver determines that one or more SMS messages is missing from an SMS transmission that they are trying to
 * receive.
 * 
 * @author benelliott, mstevens
 */
public class ResendRequestPayload extends Payload
{

	static private IntegerRangeMapping TOTAL_PARTS_FIELD = new IntegerRangeMapping(1, Math.max(BinarySMSTransmission.MAX_TRANSMISSION_PARTS, TextSMSTransmission.MAX_TRANSMISSION_PARTS));
	
	private int subjectSenderSideID;
	private int subjectPayloadHash;
	private int subjectTotalParts;
	private boolean[] requestedParts;
	
	/**
	 * 
	 * @param subject the SMSTransmission whose parts we are requesting
	 */
	public ResendRequestPayload(SMSTransmission<?> subject)
	{
		subjectSenderSideID = subject.getRemoteID();
		subjectPayloadHash = subject.getPayloadHash();
		subjectTotalParts = subject.getTotalNumberOfParts();
		requestedParts = new boolean[subjectTotalParts];
		for(int p = 0; p < subjectTotalParts; p++)
			requestedParts[p] = subject.hasPart(p + 1); // (part numbers start from 1)
	}

	@Override
	public int getType()
	{
		return BuiltinType.ResendRequest.ordinal();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		Transmission.TRANSMISSION_ID_FIELD.write(subjectSenderSideID, bitstream);
		Transmission.PAYLOAD_HASH_FIELD.write(subjectPayloadHash, bitstream);
		TOTAL_PARTS_FIELD.write(subjectTotalParts, bitstream);
		// Write "true" bit for parts that are requested and "false" bits for parts that are not:
		bitstream.write(requestedParts);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		subjectSenderSideID = Transmission.TRANSMISSION_ID_FIELD.readInt(bitstream);
		subjectPayloadHash = Transmission.PAYLOAD_HASH_FIELD.readInt(bitstream);
		subjectTotalParts = TOTAL_PARTS_FIELD.readInt(bitstream);
		requestedParts = bitstream.readBits(subjectTotalParts);
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
	
	/**
	 * Returns a List containing the part numbers of the transmission parts that not received by the receiver (i.e. those requested for retransmission).
	 * Note that part numbers start for 1, not 0.
	 * 
	 * @return a List containing the part numbers of the transmission parts that not received by the receiver (i.e. those requested for retransmission)
	 */
	public List<Integer> getRequestedPartNumbers()
	{
		List<Integer> reqPartNums = new ArrayList<Integer>();
		for(int p = 0; p < subjectTotalParts; p++)
			if(requestedParts[p])
				reqPartNums.add(p + 1); // (part numbers start from 1)
		return reqPartNums;
	}

	@Override
	public boolean acknowledgeReception()
	{
		return false; // !!!
	}
	
	@Override
	public void handle(Handler handler) throws Exception
	{
		handler.handle(this);
	}
	
}
