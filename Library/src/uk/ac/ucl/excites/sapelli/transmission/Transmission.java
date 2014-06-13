/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.io.IOException;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Abstract superclass for all Transmissions
 * 
 * TODO support for multi-schema transmissions? (containing records of more than 1 schema)
 * 
 * @author mstevens
 */
public abstract class Transmission
{

	// STATICS-------------------------------------------------------
	static public final int TRANSMISSION_ID_SIZE = 16; // bits
	static public final IntegerRangeMapping TRANSMISSION_ID_FIELD = IntegerRangeMapping.ForSize(0, TRANSMISSION_ID_SIZE); // unsigned(!) 16 bit integer
	
	// DYNAMICS------------------------------------------------------
	protected final TransmissionClient client;
	
	protected final Payload payload;
	protected Integer id = null; // Transmission ID: computed as a CRC16 hash over the transmission payload (unsigned 16 bit int)
	
	protected DateTime sentAt = null; //used only on sending side
	protected DateTime receivedAt = null; //used on receiving side, and TODO on sending side once we have acknowledgements working
	
	/**
	 * 
	 * @param client
	 * @param payload
	 */
	public Transmission(TransmissionClient client, Payload.Type payloadType)
	{
		this.client = client;
		this.payload = Payload.New(this, payloadType);
	}
	
	public void send(TransmissionSender transmissionSender) throws Exception
	{
		//Some checks:
		if(isSent())
		{
			System.out.println("This transmission (& all of its parts) has already been sent.");
			return;
		}
//		if(recordsBySchema.isEmpty())
//			throw new IllegalStateException("Transmission has no records. Add at least 1 record before sending the transmission .");
//		if(transmissionSender == null)
//			throw new IllegalStateException("Please provide a non-null TransmissionSender instance.");
		
		// Set sendingAttemptedAt for all records:
//		DateTime now = new DateTime();
//		for(Record r : records)
//			r.setSendingAttemptedAt(now);
		
		sendPayload(transmissionSender); // !!!
	}

	public void resend(TransmissionSender sender) throws Exception
	{
		// Clear early sentAt value (otherwise send() won't work):
		sentAt = null;
		
		// Resend:
		send(sender);
	}
	
	protected abstract void sendPayload(TransmissionSender transmissionSender) throws Exception;
	
	public void receive() throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException
	{
		// Some checks:
//		if(!recordsBySchema.isEmpty())
//			throw new IllegalStateException("This SMSTransmission has already been received.");
		
		// Read payload:
//		receivePayload(null, null);
	}
		
	public boolean isFull()
	{
		return payload.isFull();
	}
	
	public boolean isSent()
	{
		return sentAt != null;
	}
	
	protected void setSentAt(DateTime sentAt)
	{
//		for(List<Record> records : recordsBySchema.values())
//			for(Record r : records)
//			{
//				//TODO mark records as sent!!!
//				//r.setSent(true);
//				//r.setSendingAttemptedAt(sentAt);
//			}
		this.sentAt = sentAt;
	}
	
	public DateTime getSentAt()
	{
		return sentAt;
	}

	public boolean isReceived()
	{
		return receivedAt != null;
	}

	public DateTime getReceivedAt()
	{
		return receivedAt;
	}
	
	public void setReceivedAt(DateTime receivedAt)
	{
		this.receivedAt = receivedAt;
	}
	
	public TransmissionClient getClient()
	{
		return client;
	}
	
	public int getID()
	{	
		if(id == null)
			throw new NullPointerException("Transmission ID has not been set.");
		return id.intValue();
	}
	
	public abstract int getMaxPayloadBits();
	
	protected abstract void serialise(BitArray payloadBits) throws TransmissionCapacityExceededException, IOException;
	
	protected abstract BitArray deserialise() throws IOException;
	
}
