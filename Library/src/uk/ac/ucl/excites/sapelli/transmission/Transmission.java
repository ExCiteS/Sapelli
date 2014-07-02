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

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
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
	
	static public final int TRANSMISSION_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping TRANSMISSION_ID_FIELD = IntegerRangeMapping.ForSize(0, TRANSMISSION_ID_SIZE); // unsigned(!) 24 bit integer
	
	static public final int PAYLOAD_HASH_SIZE = 16; // bits
	static public final IntegerRangeMapping PAYLOAD_HASH_FIELD = IntegerRangeMapping.ForSize(0, PAYLOAD_HASH_SIZE); // unsigned(!) 16 bit integer
	
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
			throw new IncompleteTransmissionException(this, "Payload hash mismatch!");
		
		// Set payload:
		this.payload = Payload.New(client, payloadBits);
		this.payload.setTransmission(this); // !!!
		
		// Deserialise payload:
		payload.deserialise(payloadBits);
		
		// set receivedAT?
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
	
	public abstract int getMaxPayloadBits();
	
	/**
	 * @return whether (true) or not (false) the wrapping of the payload data in the transmission (as implemented by {@link #wrap(BitArray)}) can cause the data size to grow (e.g. due to escaping)
	 */
	public abstract boolean canWrapIncreaseSize();
	
	/**
	 * @return
	 */
	public abstract Type getType();
	
}
