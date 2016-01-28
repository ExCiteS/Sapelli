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

package uk.ac.ucl.excites.sapelli.transmission.model;

import java.io.EOFException;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.model.content.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResponsePayload;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextSMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.IncompleteTransmissionException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionReceivingException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionSendingException;

/**
 * Abstract superclass for all Transmissions
 * 
 * @author mstevens
 */
public abstract class Transmission<C extends Correspondent>
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
		GeoKey,
		// More later?
	}
	
	/**
	 * Interface for dispatching on transmission type
	 */
	static public interface Handler
	{

		public void handle(BinarySMSTransmission binSMST);
		
		public void handle(TextSMSTransmission txtSMST);
		
		public void handle(GeoKeyTransmission geoKeyT);
		
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
	
	/**
	 * Transmission format V2, which was introduced in Sapelli v2.0 betaX.
	 * This not in compatible with the format used in v1.x, which is no longer supported in Sapelli v2.0.
	 * V2 is currently the only support format but in the future variations/extension could be introduced.
	 */
	static protected final short V2_FORMAT = 2;
	
	/**
	 * The default Transmission format version being used.
	 */
	static protected final short DEFAULT_FORMAT = V2_FORMAT;
	
	/**
	 * The highest supported Transmission format version
	 */
	static protected final short HIGHEST_SUPPORTED_FORMAT = V2_FORMAT;
	
	/**
	 * We use 2 bits to store the format version This means up to 4 versions can be differentiated.
	 * Currently only 1 supported format exists (= V2). If we ever get to V5 it would be best if an
	 * additional flag is added to enable future extensions beyond V5.
	 */
	static protected final short FORMAT_VERSION_SIZE = 2; // bits
	
	/**
	 * The field used to indicate the version of the Transmission format which is being used.
	 */
	static protected final IntegerRangeMapping FORMAT_VERSION_FIELD = IntegerRangeMapping.ForSize(V2_FORMAT, FORMAT_VERSION_SIZE); // can take values from [2, 5] (but stored binary as [0, 3])
	
	static protected final int UNLIMITED_BODY_SIZE = -1;
	
	static public final int MINIMUM_RESEND_TIMEOUT_MS = 90 * 1000; // 90 seconds
	
	// DYNAMICS------------------------------------------------------
	public final TransmissionClient client;
	
	/**
	 * If {@code false} this Transmission was created on the current device for sending to another device,
	 * if {@code true} it was received on the current device by means of transmission from another one.
	 * Or in other words, if {@code false} we are on the sending side, if {@code true} we are on the receiving side.
	 * 
	 * Not to be confused with {@link #isReceived()}!
	 */
	public final boolean received;
	
	/**
	 * ID by which this transmission is identified in the context of the local device/server
	 */
	private Integer localID;
	
	/**
	 * ID by which this transmission is identified in the context of the remote device/server
	 */
	private Integer remoteID;
	
	/**
	 * The remote correspondent.
	 * On the sending side this will be the receiver, on the receiver side it will be the sender.
	 */
	protected final C correspondent;
	
	/**
	 * The payloadBitsLengthField is an IntegerRangeMapping which will be used to read/write the length of the payload data (in number of bits).<br/>
	 */
	private final IntegerRangeMapping payloadBitsLengthField;
	
	/**
	 * Contents of the transmission
	 */
	private Payload payload;
	
	/**
	 * The type of payload.
	 * Kept separately in order to know payload type without having to reconstruct Payload instance (e.g. after database retrieval).
	 */
	private Integer payloadType;
	
	/**
	 * Computed as a CRC16 hash over the transmission payload (unsigned 16 bit int).
	 * Kept separately in order to know hash value without having to reconstruct Payload instance (e.g. after database retrieval).
	 */
	private Integer payloadHash;
	
	/**
	 * used only on sending side
	 */
	private TimeStamp sentAt;
	
	/**
	 * used on receiving side, and on sending side if an acknowledgement was received
	 */
	private TimeStamp receivedAt;
	
	/**
	 * The transmission that was sent or received in response to this one.
	 */
	private Transmission<?> response;
	
	/**
	 * used only on sending side
	 */
	private transient BitArray preparedBodyBits = null;
	
	/**
	 * used only on sending side
	 */
	private transient boolean wrapped;
	
	/**
	 * To be called from the sending side.
	 * 
	 * @param client
	 * @param receiver
	 * @param payload
	 */
	public Transmission(TransmissionClient client, C receiver, Payload payload)
	{
		this(client, receiver, false); // !!!
		this.payload = payload;
		this.payload.setTransmission(this); // !!!
		this.payloadType = payload.getType();
	}
	
	/**
	 * To be called from the receiving side.
	 * 
	 * @param client
	 * @param sender
	 * @param sendingSideID
	 * @param payloadHash
	 */
	public Transmission(TransmissionClient client, C sender, int sendingSideID, int payloadHash)
	{
		this(client, sender, true); // !!!
		this.remoteID = sendingSideID;
		this.payloadHash = payloadHash;
	}
	
	/**
	 * To be called upon database retrieval only.
	 * 
	 * @param client
	 * @param correspondent
	 * @param received
	 * @param localID
	 * @param remoteID - may be null
	 * @param payloadType - may be null
	 * @param payloadHash
	 * @param sentAt - may be null
	 * @param receivedAt - may be null
	 * @param response - may be null
	 */
	protected Transmission(TransmissionClient client, C correspondent, boolean received, int localID, Integer remoteID, Integer payloadType, int payloadHash, TimeStamp sentAt, TimeStamp receivedAt, Transmission<C> response)
	{
		this(client, correspondent, received); // !!!
		this.localID = localID;
		this.remoteID = remoteID; 
		this.payloadType = payloadType;
		this.payloadHash = payloadHash;
		this.sentAt = sentAt;
		this.receivedAt = receivedAt;
		this.response = response;
	}
	
	/**
	 * To be called to create a mock received response.
	 * 
	 * @param responsePayload
	 */
	@SuppressWarnings("unchecked")
	protected Transmission(ResponsePayload responsePayload)
	{
		this(responsePayload.getSubject().client, (C) responsePayload.getSubject().correspondent, true); // !!!
		this.payload = responsePayload;
		this.payload.setTransmission(this); // !!!
		this.payloadType = payload.getType();
		this.sentAt = TimeStamp.now();
		this.receivedAt = this.sentAt;
		this.response = null;
	}
	
	/**
	 * Private constructor.
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
	private Transmission(TransmissionClient client, C correspondent, boolean received)
	{
		// Set client, correspondent & received:
		this.client = client;
		this.correspondent = correspondent;
		this.received = received;
		
		// Set payloadBitsLengthField:
		if(getMaxBodyBits() == UNLIMITED_BODY_SIZE)
		{
			this.payloadBitsLengthField = new IntegerRangeMapping(0, Integer.MAX_VALUE);
		}
		else
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
			this.payloadBitsLengthField = new IntegerRangeMapping(0, b);
		}
	}
	
	/**
	 * Only used on the sending side.
	 * 
	 * Can be overridden by subclasses.
	 * 
	 * @return a SentCallback object that can be used to register the successful sending of this transmission, its parts and its payload
	 */
	public SentCallback getSentCallback()
	{
		return new SentCallback();
	}
	
	/**
	 * @return the correspondent
	 */
	public C getCorrespondent()
	{
		return correspondent;
	}

	public boolean isLocalIDSet()
	{
		return localID != null;
	}
	
	/**
	 * @return
	 * @throws IllegalStateException when no local ID has been set
	 */
	public int getLocalID() throws IllegalStateException
	{
		if(localID == null)
			throw new IllegalStateException("LocalID has not been set yet");
		return localID.intValue();
	}
	
	/**
	 * @param localID the localID to set
	 * @throws IllegalStateException when the Transmission already has a localID which is different from the given one
	 */
	public void setLocalID(int localID) throws IllegalStateException
	{
		if(this.localID != null && this.localID.intValue() != localID)
			throw new IllegalStateException("A different localID value has already been set (existing: " + this.localID + "; new: " + localID + ")!");
		this.localID = localID;
	}
	
	public boolean isRemoteIDSet()
	{
		return remoteID != null;
	}
	
	/**
	 * @return
	 * @throws IllegalStateException when no remote ID has been set
	 */
	public int getRemoteID() throws IllegalStateException
	{
		if(remoteID == null)
			throw new IllegalStateException("RemoteID has not been set yet");
		return remoteID;
	}

	/**
	 * @param remoteID the remoteID to set
	 * @throws IllegalStateException when the Transmission already has a remoteID which is different from the given one
	 */
	public void setRemoteID(int remoteID) throws IllegalStateException
	{
		if(this.remoteID != null && this.remoteID.intValue() != remoteID)
			throw new IllegalStateException("A different remoteID value has already been set (existing: " + this.remoteID + "; new: " + remoteID + ")!");
		this.remoteID = remoteID;
	}

	public boolean isPayloadSet()
	{
		return payload != null;
	}
	
	public Payload getPayload()
	{
		if(payload == null && isComplete())
		{	// Reconstruct (i.e. "re-receive") Payload (after db retrieval):
			try
			{
				receive();
			}
			catch(TransmissionReceivingException e)
			{
				client.logError("Transmission: error upon reconstructing Payload", e);
			}
		}
		return payload;
	}
	
	public boolean isPayloadHashSet()
	{
		return payloadHash != null;
	}
	
	/**
	 * @return
	 * @throws IllegalStateException when no payload hash has been set
	 */
	public int getPayloadHash() throws IllegalStateException
	{	
		if(payloadHash == null)
			throw new IllegalStateException("Payload hash has not been set yet"); // Note: on the receiving side the hash is set before the actual payload
		return payloadHash;
	}
	
	public boolean isPayloadTypeSet()
	{
		return payload != null || payloadType != null;
	}
	
	/**
	 * @return the payloadType
	 */
	public int getPayloadType()
	{
		if(payload != null)
			return payload.getType();
		if(payloadType != null)
			return payloadType;
		return -1;
	}

	/**
	 * @param controller
	 * @throws TransmissionCapacityExceededException
	 * @throws TransmissionSendingException
	 */
	public void send(TransmissionController controller) throws TransmissionCapacityExceededException, TransmissionSendingException
	{
		// Some checks:
		if(controller == null)
			throw new TransmissionSendingException("Please provide a non-null TransmissionController instance.");
		if(isSent())
			throw new TransmissionSendingException("This transmission has already been sent.");
		
		// Prepare for sending:
		prepare(false); // (won't repeat steps that have already been performed)
		
		// Do the actual sending:
		doSend(controller);
	}
	
	/**
	 * @param controller
	 * @throws TransmissionCapacityExceededException
	 * @throws TransmissionSendingException
	 */
	public void resend(TransmissionController controller) throws TransmissionCapacityExceededException, TransmissionSendingException
	{
		// Clear earlier sentAt value (otherwise send() won't work):
		sentAt = null;
		
		// Resend:
		send(controller);
	}
	
	/**
	 * @param transmissionController
	 * @throws TransmissionSendingException
	 */
	protected abstract void doSend(TransmissionController controller) throws TransmissionSendingException;
	
	/**
	 * @throws TransmissionCapacityExceededException
	 * @throws TransmissionSendingException
	 */
	public void checkCapacity() throws TransmissionCapacityExceededException, TransmissionSendingException
	{
		if(isBodySizeUnlimited())
			return; // there's no point in checking capacity if body size is unlimited
		
		// Clear previous preparations (necessary because payload contents have likely changed):
		clearPreparation();
		
		// Prepare for simulation:
		try
		{
			prepare(true);
		}
		catch(TransmissionSendingException e)
		{
			clearPreparation();
			throw e;
		}
	}
	
	/**
	 * Prepares the transmission for storage and/or sending.
	 * 
	 * @throws TransmissionCapacityExceededException
	 * @throws TransmissionSendingException
	 */
	public void prepare() throws TransmissionCapacityExceededException, TransmissionSendingException
	{
		prepare(false);
	}
	
	/**
	 * Prepares the transmission for sending, storage or capacity checking.
	 * 
	 * @param simulation whether or not the is a simulation (for capacity checking) or a full preparation (for storage/sending)
	 * @throws TransmissionCapacityExceededException
	 * @throws TransmissionSendingException
	 */
	private void prepare(boolean simulation) throws TransmissionCapacityExceededException, TransmissionSendingException
	{
		//Some checks:
		if(payload == null || payloadType == null)
			throw new TransmissionSendingException("Cannot prepare/store/send transmission without payload");
		
		try
		{
			// Prepare body bits if needed (includes Payload serialisation):
			if(preparedBodyBits == null || payloadHash == null)
			{
				// Open input stream:
				BitArrayOutputStream bitstream = new BitArrayOutputStream();
				
				//  Format version (2 bits):
				FORMAT_VERSION_FIELD.write(DEFAULT_FORMAT, bitstream);
				
				// TODO anonymous / user-cred (maybe only for next transmission format version?)
				// TODO encrypted flag + encryption-related fields (maybe only for next transmission format version?)
				
				// Write payload type:
				Payload.PAYLOAD_TYPE_FIELD.write(payload.getType(), bitstream);
				
				// Get serialised payload bits:
				BitArray payloadBits = payload.serialise();
				
				// Capacity check:
				if(payloadBits.length() > getMaxPayloadBits())
					throw new TransmissionCapacityExceededException("Payload is too large for the associated transmission (size: " + payloadBits.length() + " bits; max for this type of transmission: " + getMaxPayloadBits() + " bits");
				
				// Compute & store payload hash:
				this.payloadHash = computePayloadHash(payloadBits);
				
				// Write payload bits length:
				payloadBitsLengthField.write(payloadBits.length(), bitstream);
				
				// Write the actual payload bits:
				bitstream.write(payloadBits);
				
				// Flush, close & get body bits:
				bitstream.flush();
				bitstream.close();
				preparedBodyBits = bitstream.toBitArray();
			}
			
			// Wrap if needed:
			if(!wrapped && (!simulation || canWrapIncreaseSize()))
			{
				wrap(preparedBodyBits);
				wrapped = true;
				preparedBodyBits = null; // no need to keep this, so wipe to reduce memory usage
			}
		}
		catch(IOException ioe)
		{
			throw new TransmissionSendingException("Error upon preparing transmission", ioe);
		}
	}
	
	public void clearPreparation()
	{
		payloadHash = null;
		preparedBodyBits = null;
		wrapped = false;
	}
	
	/**
	 * @throws TransmissionReceivingException
	 */
	public void receive() throws TransmissionReceivingException
	{
		// Some checks:
		if(!isComplete())
			throw new IncompleteTransmissionException(this);
		if(this.payload != null)
		{
			client.logWarning("This transmission has already been received.");
			return;
		}
		
		// Decode transmission & payload:
		try
		{
			// Unwrap (reassemble/decode) body:
			BitArray bodyBits = unwrap();
			
			// Length check:
			if(bodyBits.length() < MIN_BODY_LENGTH_BITS - 1) // - 1 because in an extreme case it could be that the payload data is empty (0 bits long)
				throw new IncompleteTransmissionException(this, "Transmission body length (" + bodyBits.length() + " bits) for this to be a valid transmission.");
			
			// Open input stream:
			BitArrayInputStream bitstream = new BitArrayInputStream(bodyBits);
			
			short format = FORMAT_VERSION_FIELD.readShort(bitstream);
			if(format > HIGHEST_SUPPORTED_FORMAT)
				throw new TransmissionReceivingException(this, "Unsupported transmission format version: " + format + " (highest supported version: " + HIGHEST_SUPPORTED_FORMAT + ").");
			
			// Read payload type & instantiate Payload object:
			this.payloadType = Payload.PAYLOAD_TYPE_FIELD.readInt(bitstream);
			this.payload = Payload.New(client, payloadType);
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
				throw new TransmissionReceivingException(this, "Payload hash mismatch!");
			
			// Deserialise payload:
			payload.deserialise(payloadBits); // does not throw exception, instead any exception can be accessed through Payload.getDecodeException()
		}
		catch(Exception e)
		{
			if(e instanceof TransmissionReceivingException)
				throw (TransmissionReceivingException) e;
			throw new TransmissionReceivingException(this, "Transmission could not be received/decoded", e);
		}
	}
	
	/**
	 * The maximum length of the body of this transmission (in number of bits),
	 * or {@value #UNLIMITED_BODY_SIZE} if there is no practical limit on the body of this transmission.
	 * 
	 * @return
	 */
	protected abstract int getMaxBodyBits();
	
	/**
	 * @return whether or not the body size is unlimited
	 */
	public boolean isBodySizeUnlimited()
	{
		return getMaxBodyBits() == UNLIMITED_BODY_SIZE;
	}
	
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
	
	/**
	 * To be overridden.
	 * 
	 * @return
	 */
	public boolean isPartiallySent()
	{
		return isSent();
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
	
	/**
	 * @return the response
	 */
	public Transmission<?> getResponse()
	{
		return response;
	}

	/**
	 * @param response the response to set
	 */
	public void setResponse(Transmission<?> response)
	{
		if(this == response)
			throw new IllegalArgumentException("Transmission cannot be its own response!");
		this.response = response;
	}
	
	public boolean hasResponse()
	{
		return response != null;
	}

	/**
	 * May be overridden.
	 * 
	 * @return whether it is appropriate to resend (the payload of) this transmission *now*
	 */
	public boolean isResendAppropriate()
	{
		return	// never actually sent (not even partially):
				!isPartiallySent() ||
				// never received (or no ACK received, nor any other response)
				(	!isReceived() && !hasResponse() &&
					sentAt.shift(getApproprateResentTimeoutMS()).isBefore(TimeStamp.now()));
	}
	
	public int getApproprateResentTimeoutMS()
	{
		return MINIMUM_RESEND_TIMEOUT_MS;
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
	
	/**
	 * Class responsible for implementing what needs to happen when the Transmission (or parts of it) have been sent.
	 * 
	 * @author mstevens
	 */
	public class SentCallback extends Callback
	{

		protected void store()
		{
			store(Transmission.this);
		}
		
		public void onSent()
		{
			onSent(TimeStamp.now());
		}
		
		public void onSent(TimeStamp sentAt)
		{
			try
			{
				setSentAt(sentAt);
				
				// Run payload callback if there is one:
				if(payload != null /*just in case*/ && payload.getCallback() != null)
					payload.getCallback().onSent(sentAt);
				
				// Store updated transmission:
				store();
			}
			catch(Exception e)
			{
				client.logError("Error in SentCallback#onSent()", e);
			}
		}
		
		public void onReceived()
		{
			onSent(TimeStamp.now());
		}
		
		/**
		 * Handles a ResponsePayload received (or mocked) in response to this transmission.
		 * 
		 * @param responsePayload
		 */
		public void onResponse(ResponsePayload responsePayload)
		{
			if(	responsePayload == null ||
				(responsePayload.getSubject() != null && !Objects.equals(Transmission.this.localID, responsePayload.getSubject().localID)))
				return; // should never happen
			
			// Set transmission used to bring us the responsePayload as this transmission's response:
			setResponse(responsePayload.transmission);
			
			// Set remote ID if we got it:
			if(responsePayload.getSubjectReceiverSideID() != null)
				setRemoteID(responsePayload.getSubjectReceiverSideID());
			
			// If is is an ACK response...
			if(responsePayload instanceof AckPayload)
				// ... mark subject as (successfully) received:
				setReceivedAt(responsePayload.getSubjectReceivedAt());
			
			// Store updated transmission:
			store();
		}
		
		public void setMockResponse(Transmission<C> mockResponseTransmission)
		{
			if(!(mockResponseTransmission.payload instanceof ResponsePayload))
				return; // should never happen
			
			try
			{
				// Prepare mock transmission for storage:
				mockResponseTransmission.prepare();
				
				// Store the mock transmission:
				store(mockResponseTransmission);
				
				// Handle responsePayload:
				onResponse((ResponsePayload) mockResponseTransmission.payload);
			}
			catch(Exception e)
			{
				client.logError("Error in " + Transmission.class.getSimpleName() + "#setMockResponse()", e);
			}
		}
		
	}
	
}
