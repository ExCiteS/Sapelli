/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.io.IOException;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.util.IncompleteTransmissionException;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Abstract superclass for all Transmissions
 * 
 * @author mstevens
 */
public abstract class Transmission
{

	// STATICS-------------------------------------------------------
	static public final int SEQUENTIAL_ID_SIZE = 6; // bits
	static public final IntegerRangeMapping SEQUENTIAL_ID_FIELD = IntegerRangeMapping.ForSize(0, SEQUENTIAL_ID_SIZE); // unsigned(!) 6 bit integer
	
	static public final int PAYLOAD_HASH_SIZE = 16; // bits
	static public final IntegerRangeMapping PAYLOAD_HASH_FIELD = IntegerRangeMapping.ForSize(0, PAYLOAD_HASH_SIZE); // unsigned(!) 16 bit integer
	
	static private final int PAYLOAD_HASH_NOT_SET = -1;
	
	// DYNAMICS------------------------------------------------------
	protected final TransmissionClient client;
	
	/**
	 * ID by which this transmission is identified in the context of the local device/server storage alone
	 */
	protected Integer localID;
	
	/**
	 * Contents of the transmission
	 */
	protected Payload payload;
	
	/**
	 * Computed as a CRC16 hash over the transmission payload (unsigned 16 bit int)
	 */
	protected int payloadHash = PAYLOAD_HASH_NOT_SET;
	
	protected DateTime sentAt = null; //used only on sending side
	protected DateTime receivedAt = null; //used on receiving side, and TODO on sending side once we have acknowledgements working
	
	/**
	 * To be called from the sending side
	 * 
	 * @param client
	 * @param payload
	 */
	public Transmission(TransmissionClient client, Payload payload)
	{
		this.client = client;
		this.payload = payload;
		this.payload.setTransmission(this); // just in case
	}
	
	/**
	 * To be called from the receiving side
	 * 
	 * @param client
	 * @param payloadHash
	 */
	public Transmission(TransmissionClient client, int payloadHash)
	{
		this.client = client;
		this.payloadHash = payloadHash;
	}
	
	public void setLocalID(int id)
	{
		if(localID != null && localID != id)
			throw new IllegalStateException("LocalID has already been set!");
		this.localID = id;
	}
	
	public int getLocalID()
	{
		if(localID == null)
			throw new IllegalStateException("LocalID has not been set yet");
		return localID.intValue();
	}
	
	public Payload getPayload()
	{
		return payload;
	}
	
	public int getPayloadHash()
	{	
		if(payloadHash == PAYLOAD_HASH_NOT_SET)
			throw new IllegalStateException("Payload hash has not been set yet");
		return payloadHash;
	}
	
	public void send(Sender transmissionSender) throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		//Some checks:
		if(transmissionSender == null)
			throw new IllegalStateException("Please provide a non-null TransmissionSender instance.");
		if(payload == null)
			throw new NullPointerException("Cannot send transmission without payload");
		if(isSent())
		{
			System.out.println("This transmission has already been sent.");
			return;
		}
		
		// Serialise payload:
		BitArray payloadBits = payload.serialise();
		
		// Compute & store payload hash:
		this.payloadHash = computePayloadHash(payloadBits); // must be set before wrap() is called!
		
		// Wrap payload for transmission:
		wrap(payloadBits);
		
		// The actual sending:
		doSend(transmissionSender);
	}

	public void resend(Sender sender) throws Exception
	{
		// Clear early sentAt value (otherwise send() won't work):
		sentAt = null;
		
		// Resend:
		send(sender);
	}
	
	protected abstract void doSend(Sender transmissionSender);
	
	public void receive() throws IncompleteTransmissionException, IOException, IllegalStateException, PayloadDecodeException, UnknownModelException
	{
		// Some checks:
		if(!isComplete())
			throw new IncompleteTransmissionException(this);
		if(this.payload != null)
		{
			System.out.println("This transmission has already been received.");
			return;
		}
		
		// Unwrap (reassemble/decode) payload:
		BitArray payloadBits = unwrap();
		
		// Verify payload hash:
		if(payloadHash != computePayloadHash(payloadBits))
			throw new IncompleteTransmissionException(this, "Payload hash mismatch");
		
		// Set payload:
		this.payload = Payload.New(client, payloadBits);
		this.payload.setTransmission(this); // !!!
		
		// Deserialise payload:
		payload.deserialise(payloadBits);
	}
	
	/**
	 * Wraps/encodes/splits the payload bits in a way they can be send by this transmission
	 * 
	 * @param payloadBits
	 * @throws TransmissionCapacityExceededException
	 * @throws IOException
	 */
	protected abstract void wrap(BitArray payloadBits) throws TransmissionCapacityExceededException, IOException;
	
	/**
	 * Unwraps/decoded/joins the payload bits
	 * 
	 * @return
	 * @throws IOException
	 */
	protected abstract BitArray unwrap() throws IOException;
	
	protected int computePayloadHash(BitArray payloadBits)
	{
		return Hashing.getCRC16Hash(payloadBits.toByteArray());
	}
	
	public abstract boolean isComplete();
		
	public boolean isSent()
	{
		return sentAt != null;
	}
	
	protected void setSentAt(DateTime sentAt)
	{
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
	
	public abstract int getMaxPayloadBits();
	
}
