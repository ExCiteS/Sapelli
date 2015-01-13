/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.transmission;

import java.io.EOFException;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextSMSTransmission;
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
	/**
	 * The different (concrete) transmission types, reflecting different networks or modes of transport. 
	 */
	public static enum Type
	{
		BINARY_SMS,
		TEXTUAL_SMS,
		HTTP,
		// More later?
	}
	
	/**
	 * Interface for dispatching on transmission type
	 */
	static public interface Handler
	{

		public void handle(BinarySMSTransmission binSMST);
		
		public void handle(TextSMSTransmission txtSMST);
		
		public void handle(HTTPTransmission httpT);
		
	}
	
	static public final int TRANSMISSION_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping TRANSMISSION_ID_FIELD = IntegerRangeMapping.ForSize(0, TRANSMISSION_ID_SIZE); // unsigned(!) 24 bit integer
	
	static public final int PAYLOAD_HASH_SIZE = 16; // bits
	static public final IntegerRangeMapping PAYLOAD_HASH_FIELD = IntegerRangeMapping.ForSize(0, PAYLOAD_HASH_SIZE); // unsigned(!) 16 bit integer
	
	/**
	 * Minimum number of bits needed to fit transmission body, composed of:
	 * 	- payload type field: Payload.PAYLOAD_TYPE_SIZE (= 5) bits
	 * 	- payload data bits length field: at least 1 bit
	 * 	- the actual payload data bits: at least 1 bit  
	 */
	static private final int MIN_BODY_LENGTH_BITS = Payload.PAYLOAD_TYPE_SIZE + 1 + 1; // bits
	
	static public final int CORRESPONDENT_MAX_LENGTH = 256;
	
	// DYNAMICS------------------------------------------------------
	protected final TransmissionClient client;
	
	/**
	 * ID by which this transmission is identified in the context of the local device/server
	 */
	protected Integer localID;
	
	/**
	 * ID by which this transmission is identified in the context of the remote device/server
	 */
	protected Integer remoteID;
	
	/**
	 * The payloadBitsLengthField is an IntegerRangeMapping which will be used to read/write the length of the payload data (in number of bits).<br/>
	 * It is initialised in {@link #initialise()}.
	 */
	private IntegerRangeMapping payloadBitsLengthField = null;
	
	/**
	 * Contents of the transmission
	 */
	protected Payload payload;
	
	/**
	 * Computed as a CRC16 hash over the transmission payload (unsigned 16 bit int)
	 */
	protected Integer payloadHash;
	
	protected TimeStamp sentAt; //used only on sending side
	protected TimeStamp receivedAt; //used on receiving side, and on sending side if an acknowledgement was received
	
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
		initialise(); // !!!
	}
	
	/**
	 * To be called from the receiving side
	 * 
	 * @param client
	 * @param sendingSideID
	 * @param payloadHash
	 */
	public Transmission(TransmissionClient client, int sendingSideID, int payloadHash)
	{
		this.client = client;
		this.remoteID = sendingSideID;
		this.payloadHash = payloadHash;
		initialise(); // !!!
	}
	
	/**
	 * To be called upon database retrieval only
	 * 
	 * @param client
	 * @param localID
	 * @param remoteID - may be null
	 * @param payloadHash
	 * @param sentAt - may be null
	 * @param receivedAt - may be null
	 */
	protected Transmission(TransmissionClient client, int localID, Integer remoteID, int payloadHash, TimeStamp sentAt, TimeStamp receivedAt)
	{
		this.client = client;
		this.localID = localID;
		this.remoteID = remoteID; 
		this.payloadHash = payloadHash;
		this.sentAt = sentAt;
		this.receivedAt = receivedAt;
		initialise(); // !!!
	}
	
	/**
	 * Initialise method.
	 * 
	 * Instantiates the payloadBitsLengthField: an IntegerRangeMapping which is used to read/write the length of the
	 * payload data (in number of bits). The size of this field is chosen such that it is *just* big enough space to hold
	 * values of 0 up to (and including) the maximum number of payload data bits the transmission can contain. The latter
	 * value is equal to total transmission body capacity (given by getMaxBodyBits()) decreased by the size of the
	 * PAYLOAD_TYPE_FIELD and the size of the payloadBitsLengthField itself!
	 * Hence, computing the field's size (and/or highbound) is a kind of "chicken or the egg" problem. The answer lies in
	 * in the logarithmic equation explained in the method code.
	 * 
	 * @return the payloadBitsLengthField
	 */
	private void initialise()
	{
		// Transmission capacity check:
		if(getMaxBodyBits() < MIN_BODY_LENGTH_BITS)
			throw new IllegalStateException("Transmission capacity (max body size) is too small! It is " + getMaxBodyBits() + " bits; while the minimum is " + MIN_BODY_LENGTH_BITS + " bits.");
		
		// Helper variable: a = bits available for length field (at least 1) + the actual data bits (at least 1)
		int a = getMaxBodyBits() - Payload.PAYLOAD_TYPE_SIZE;
		
		/* The payloadBitlengthField must be *just* big enough to contain values from [0, b], where b (the field's
		 * 	strict highbound) is the maximum number of actual payload data bits with can store.
		 * This means that:
		 * 												/ 0	in most cases --> no bits wasted
		 * 		a - payloadBitlengthField.size() - b = { 
		 * 												\ 1	in rare cases (6 times for 2 <= a <= 100) --> 1 bit wasted
		 * This is achieved by the following equation:
		 * 		b = a - floor(log2(a - floor(log2(a)))) - 1
		 * However, because ...
		 * 		floor(log2(x)) = 31 - Integer.numberOfLeadingZeros(x) = Integer.SIZE - 1 - Integer.numberOfLeadingZeros(x)
		 * ... this equation can be reduced to: */
		int b = a - Integer.SIZE + Integer.numberOfLeadingZeros(a - Integer.SIZE + 1 + Integer.numberOfLeadingZeros(a));
		
		// Construct payloadBitsLengthField:
		payloadBitsLengthField = new IntegerRangeMapping(0, b);
	}
	
	public void setLocalID(int id)
	{
		if(localID != null && localID != id)
			throw new IllegalStateException("LocalID has already been set!");
		this.localID = id;
	}
	
	public boolean isLocalIDSet()
	{
		return localID != null;
	}
	
	public int getLocalID()
	{
		if(localID == null)
			throw new IllegalStateException("LocalID has not been set yet");
		return localID.intValue();
	}
		
	/**
	 * @return the remoteID
	 */
	public int getRemoteID()
	{
		if(remoteID == null)
			throw new IllegalStateException("RemoteID has not been set yet");
		return remoteID;
	}

	public boolean isPayloadSet()
	{
		return payload != null;
	}
	
	public Payload getPayload()
	{
		return payload;
	}
	
	public int getPayloadHash()
	{	
		if(payloadHash == null)
			throw new IllegalStateException("Payload hash has not been set yet"); // Note: on the receiving side the hash is set before the actual payload
		return payloadHash;
	}
	
	public void send(TransmissionController transmissionSender) throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		if(transmissionSender == null)
			throw new IllegalStateException("Please provide a non-null TransmissionSender instance.");
		prepareAndSend(transmissionSender);
	}
	
	public void checkCapacity() throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		prepareAndSend(null);
	}
	
	/**
	 * @param transmissionSender pass null for capacity check
	 * @throws IOException
	 * @throws TransmissionCapacityExceededException
	 * @throws UnknownModelException
	 */
	private void prepareAndSend(TransmissionController transmissionSender) throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		//Some checks:
		if(payload == null)
			throw new NullPointerException("Cannot send transmission without payload");
		if(isSent())
		{
			System.out.println("This transmission has already been sent.");
			return;
		}
		
		// Open input stream:
		BitArrayOutputStream bitstream = new BitArrayOutputStream();
		
		// TODO transmission format version !!!
		
		// TODO anonymous / user-cred (maybe only for next transmission format version?)
		// TODO encrypted flag + encryption-related fields (maybe only for next transmission format version?)
		
		// Write payload type:
		Payload.PAYLOAD_TYPE_FIELD.write(payload.getType(), bitstream);
		
		// Get serialised payload bits:
		BitArray payloadBits = payload.serialise();
		
		// Capacity check:
		if(payloadBits.length() > getMaxPayloadBits())
			throw new TransmissionCapacityExceededException("Payload is too large for the associated transmission (size: " + payloadBits.length() + " bits; max for this type of transmission: " + getMaxPayloadBits() + " bits");
		
		// Abort capacity check if possible:
		if(transmissionSender == null && !canWrapIncreaseSize())
			return;
		
		// Compute & store payload hash:
		this.payloadHash = computePayloadHash(payloadBits); // must be set before wrap() is called!	
		
		// Write payload bits length:
		payloadBitsLengthField.write(payloadBits.length(), bitstream);
		
		// Write the actual payload bits:
		bitstream.write(payloadBits);
		
		// Flush, close & get body bits:
		bitstream.flush();
		bitstream.close();
		BitArray bodyBits = bitstream.toBitArray();
		
		// Wrap body for transmission:
		wrap(bodyBits); // note: payloadHash must be set before this call
		
		// Do the actual sending:
		if (transmissionSender != null)
			doSend(transmissionSender);
	}

	public void resend(TransmissionController sender) throws Exception
	{
		// Clear early sentAt value (otherwise send() won't work):
		sentAt = null;
		
		// Resend:
		send(sender);
	}

	protected abstract void doSend(TransmissionController transmissionSender);
	
	public void receive() throws IncompleteTransmissionException, IOException, IllegalArgumentException, IllegalStateException, PayloadDecodeException, UnknownModelException
	{
		// Some checks:
		if(!isComplete())
			throw new IncompleteTransmissionException(this);
		if(this.payload != null)
		{
			System.out.println("This transmission has already been received.");
			return;
		}
		
		// Unwrap (reassemble/decode) body:
		BitArray bodyBits = unwrap();
		
		// Length check:
		if(bodyBits.length() < MIN_BODY_LENGTH_BITS - 1) // - 1 because in an extreme case it could be that the payload data is empty (0 bits long)
			throw new IncompleteTransmissionException(this, "Transmission body length (" + bodyBits.length() + " bits) for this to be a valid transmission.");
		
		// Open input stream:
		BitArrayInputStream bitstream = new BitArrayInputStream(bodyBits);
		
		// Read payload type & instantiate Payload object:
		this.payload = Payload.New(client, Payload.PAYLOAD_TYPE_FIELD.readInt(bitstream));
		this.payload.setTransmission(this); // !!!
		
		// Read payload bits length:
		int payloadBitsLength = payloadBitsLengthField.readInt(bitstream);
		
		// Read the actual payload bits:
		BitArray payloadBits;
		try
		{
			payloadBits = bitstream.readBitArray(payloadBitsLength);
		}
		catch(EOFException eofe)
		{	// not enough bits could be read (i.e. less than payloadBitsLength)
			throw new IncompleteTransmissionException(this, "Transmission body is incomplete, could not read all of the expected " + payloadBitsLength + " payload data bits.");
		}
		
		// Close stream:
		bitstream.close();
		
		// Verify payload hash:
		if(payloadHash != computePayloadHash(payloadBits))
			throw new IncompleteTransmissionException(this, "Payload hash mismatch!");
		
		// Deserialise payload:
		payload.deserialise(payloadBits);
		
		// TODO set receivedAT?
	}
	
	/**
	 * The maximum length of the body of this transmission (in number of bits).
	 * 
	 * @return
	 */
	protected abstract int getMaxBodyBits();
	
	/**
	 * The maximum length of the payload data (in number of bits) which can be fitted into this transmission.
	 * 
	 * @return
	 */
	public int getMaxPayloadBits()
	{
		return payloadBitsLengthField.highBound(true).intValue();
	}
	
	/**
	 * Wraps/encodes/splits the payload bits in a way they can be send by this transmission
	 * 
	 * @param bodyBits
	 * @throws TransmissionCapacityExceededException
	 * @throws IOException
	 */
	protected abstract void wrap(BitArray bodyBits) throws TransmissionCapacityExceededException, IOException;
	
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
	
	protected void setSentAt(TimeStamp sentAt)
	{
		this.sentAt = sentAt;
	}
	
	public TimeStamp getSentAt()
	{
		return sentAt;
	}

	public boolean isReceived()
	{
		return receivedAt != null;
	}

	public TimeStamp getReceivedAt()
	{
		return receivedAt;
	}
	
	public void setReceivedAt(TimeStamp receivedAt)
	{
		this.receivedAt = receivedAt;
	}
	
	public TransmissionClient getClient()
	{
		return client;
	}
	
	/**
	 * @return whether (true) or not (false) the wrapping of the payload data in the transmission (as implemented by {@link #wrap(BitArray)}) can cause the data size to grow (e.g. due to escaping)
	 */
	public abstract boolean canWrapIncreaseSize();
	
	/**
	 * @return
	 */
	public abstract Type getType();
	
	/**
	 * @param handler
	 */
	public abstract void handle(Handler handler);
	
}
