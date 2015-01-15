package uk.ac.ucl.excites.sapelli.transmission.payloads;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanListColumn;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Payload class for resend requests to be used when the receiver determines that one or more SMS messages is missing from an SMS transmission that they are trying to
 * receive.
 * 
 * @author benelliott
 *
 */
public class ResendRequestPayload extends Payload
{
	// Use a boolean list column to read and write the "part received" flag bits in the payload body:
	private static final BooleanListColumn RECEIVED_PARTS_FLAGS_COL = new BooleanListColumn("receivedFlags", false, 1, SMSTransmission.MAX_NUM_PARTS);
	
	private int subjectSenderSideID;
	private int subjectPayloadHash;
	// maintain the list of parts requested as an integer array (of part indices) for convenience but convert to/from boolean flags when writing/reading the payload
	private List<Integer> partsRequested;
	
	/**
	 * 
	 * @param subject the SMSTransmission whose parts we are requesting
	 */
	public ResendRequestPayload(SMSTransmission<?> subject) {
		this.subjectSenderSideID = subject.getRemoteID();
		this.subjectPayloadHash = subject.getPayloadHash();
		partsRequested = new ArrayList<Integer>();
		for (int i = 0; i < subject.getTotalNumberOfParts(); i++)
		{
			if (subject.getPart(i) == null)
				partsRequested.add(i);
		}
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
		// turn our List<Integer> of part indices into a List<Boolean> of "presence" flags
		List<Boolean> receivedFlags = new ArrayList<Boolean>();
		for (int i = 0; i < partsRequested.size(); i++)
		{
			// if index is present in partsRequested then put a "false" flag, else put a "true"
			receivedFlags.add(!(partsRequested.contains(i)));
		}
		RECEIVED_PARTS_FLAGS_COL.writeValue(receivedFlags, bitstream);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		subjectSenderSideID = Transmission.TRANSMISSION_ID_FIELD.readInt(bitstream);
		subjectPayloadHash = Transmission.PAYLOAD_HASH_FIELD.readInt(bitstream);
		List<Boolean> receivedFlags = RECEIVED_PARTS_FLAGS_COL.readValue(bitstream);
		// create a List<Integer> of part indices from the "presence" flags in the payload body 
		partsRequested = new ArrayList<Integer>();
		for (int i = 0; i < receivedFlags.size(); i++)
		{
			// add indices to partsReceived for parts that are marked "true" in the payload body
			if (!receivedFlags.get(i))
				partsRequested.add(i);
		}
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
	 * @return a List containing the part numbers of the transmission parts that not received by the receiver (i.e. those requested for retransmission)
	 */
	public List<Integer> getPartsRequested()
	{
		return partsRequested;				
	}

}
