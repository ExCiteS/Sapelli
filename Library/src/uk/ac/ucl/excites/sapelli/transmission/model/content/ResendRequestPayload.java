package uk.ac.ucl.excites.sapelli.transmission.model.content;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
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
public class ResendRequestPayload extends ResponsePayload
{

	static private IntegerRangeMapping TOTAL_PARTS_FIELD = new IntegerRangeMapping(1, Math.max(BinarySMSTransmission.MAX_TRANSMISSION_PARTS, TextSMSTransmission.MAX_TRANSMISSION_PARTS));
	
	private int subjectTotalParts;
	private boolean[] requestedParts;
	
	/**
	 * To be called from receiving side
	 */
	public ResendRequestPayload()
	{
		super();
	}
	
	/**
	 * To be called from sending side (= which received the subject)
	 * 
	 * @param subject the incomplete SMSTransmission whose parts we are requesting
	 * @param controller
	 */
	public ResendRequestPayload(SMSTransmission<?> subject, TransmissionController controller)
	{
		super(subject);
		
		subjectTotalParts = subject.getTotalNumberOfParts();
		requestedParts = new boolean[subjectTotalParts];
		for(int p = 0; p < subjectTotalParts; p++)
			requestedParts[p] = subject.hasPart(p + 1); // (part numbers start from 1)
		
		// Set-up SentCallback:
		setCallback(new ResentRequestSentCallback(subject, controller));
	}

	@Override
	public int getType()
	{
		return BuiltinType.ResendRequest.ordinal();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException
	{
		super.write(bitstream);
		TOTAL_PARTS_FIELD.write(subjectTotalParts, bitstream);
		// Write "true" bit for parts that are requested and "false" bits for parts that are not:
		bitstream.write(requestedParts);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		super.read(bitstream);
		subjectTotalParts = TOTAL_PARTS_FIELD.readInt(bitstream);
		requestedParts = bitstream.readBits(subjectTotalParts);
	}
	
	/**
	 * @return the subjectTotalParts
	 */
	public int getSubjectTotalParts()
	{
		return subjectTotalParts;
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
	
	/**
	 * @author mstevens
	 */
	private class ResentRequestSentCallback extends SentCallback implements StoreUser
	{
		
		private final SMSTransmission<?> incompleteT;
		private final TransmissionController controller;
		
		/**
		 * @param incompleteTransmission
		 */
		public ResentRequestSentCallback(SMSTransmission<?> incompleteTransmission, TransmissionController controller)
		{
			this.incompleteT = incompleteTransmission;
			this.controller = controller;
		}

		@Override
		public void onSent(TimeStamp sentAt)
		{
			// Remember the resent request was sent:
			incompleteT.setNumberOfSentResendRequests(incompleteT.getNumberOfSentResendRequests() + 1);
			incompleteT.setLastResendRequestSentAt(sentAt);
			
			// Store updated transmission:
			TransmissionClient client = incompleteT.getClient();
			try
			{
				client.receivedTransmissionStoreHandle.getStore(this).store(incompleteT);
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
			}
			finally
			{
				client.receivedTransmissionStoreHandle.doneUsing(this);
			}
			
			// Schedule next request (won't do anything if max reached):
			controller.scheduleSMSResendRequest(incompleteT.getLocalID(), incompleteT.getNextResendRequestSendingTime());
		}
		
	}
	
}
