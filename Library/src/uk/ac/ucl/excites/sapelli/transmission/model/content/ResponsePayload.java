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

package uk.ac.ucl.excites.sapelli.transmission.model.content;

import java.io.IOException;

import org.joda.time.DateTimeZone;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
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
	
	/**
	 * Only set on sending side and not after db retrieval.
	 */
	protected Transmission<?> subject;
	
	protected int subjectSenderSideID;
	protected int subjectPayloadHash;
	protected Integer subjectReceiverSideID;

	protected TimeStamp subjectReceivedAt;
	
	/**
	 * To be called from sending side (the side sending the response, = the receiver of the subject)
	 */
	public ResponsePayload(Transmission<?> subject)
	{
		super();
		this.subject = subject;
		this.subjectSenderSideID = subject.getRemoteID();
		this.subjectPayloadHash = subject.getPayloadHash();
		this.subjectReceiverSideID = subject.isLocalIDSet() ? subject.getLocalID() : null;
		this.subjectReceivedAt = subject.getReceivedAt();
	}
	
	/**
	 * To be called from receiving side or upon db retrieval.
	 */
	public ResponsePayload()
	{
		super();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException
	{
		Transmission.TRANSMISSION_ID_FIELD.write(subjectSenderSideID, bitstream);
		Transmission.PAYLOAD_HASH_FIELD.write(subjectPayloadHash, bitstream);
		if(bitstream.write(subjectReceiverSideID != null)) // write presence bit for subjectReceiverSideID
			Transmission.TRANSMISSION_ID_FIELD.write(subjectReceiverSideID, bitstream);
		TransmissionStore.COLUMN_RECEIVED_AT.writeValue(subjectReceivedAt, bitstream, true);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException
	{
		subjectSenderSideID = Transmission.TRANSMISSION_ID_FIELD.readInt(bitstream);
		subjectPayloadHash = Transmission.PAYLOAD_HASH_FIELD.readInt(bitstream);
		subjectReceiverSideID = bitstream.readBit() ? Transmission.TRANSMISSION_ID_FIELD.readInt(bitstream) : null; // read presence bit, and value if presence = 1/true
		subjectReceivedAt = new TimeStamp(TransmissionStore.COLUMN_RECEIVED_AT.readValue(bitstream, true).getMsSinceEpoch(), DateTimeZone.getDefault());
		// the TimeStamp read by the column is in UTC so we convert to local timezone (of this device, i.e. the receiver) to match the other timestamps stored with transmissions
	}
	
	/**
	 * @return the subject - may be null
	 */
	public Transmission<?> getSubject()
	{
		return subject;
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
	 * This returns the "subjectReceiverSideID", which is the local ID from the p.o.v. of the receiver of the subject (= sender of the response),
	 * or the remote ID from the p.o.v. of the sender of the subject (= receiver of the response).
	 * 
	 * @return the subjectReceiverSideID, may be null if the subject was not stored by the receiver before it sent this response 
	 */
	public Integer getSubjectReceiverSideID()
	{
		return subjectReceiverSideID;
	}

	/**
	 * @return the subjectReceivedAt
	 */
	public TimeStamp getSubjectReceivedAt()
	{
		return subjectReceivedAt;
	}
	
}
