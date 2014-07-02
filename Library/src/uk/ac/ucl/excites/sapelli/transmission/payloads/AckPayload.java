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

package uk.ac.ucl.excites.sapelli.transmission.payloads;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * A Payload used to acknowledge the successful reception (& decoding, processing, etc.) of an other transmission.
 * The receiver of that "subject" transmission will send this payload back to the original sender.
 * 
 * @author mstevens
 */
public class AckPayload extends Payload
{

	private int subjectSenderSideID;
	private int subjectPayloadHash;
	private TimeStamp subjectReceivedAt;
	
	/**
	 * To be called from receiving side
	 * 
	 */
	public AckPayload()
	{
		super();
	}

	/**
	 * To be called from sending side (= which received the subject)
	 * 
	 * @param subject - the transmission whose successful reception (& decoding, processing, etc.) is being acknowledged
	 */
	public AckPayload(Transmission subject)
	{
		this.subjectSenderSideID = subject.getRemoteID();
		this.subjectPayloadHash = subject.getPayloadHash();
		this.subjectReceivedAt = subject.getReceivedAt();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Payload#getType()
	 */
	@Override
	public int getType()
	{
		return BuiltinType.Ack.ordinal();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException
	{
		Transmission.TRANSMISSION_ID_FIELD.write(subjectSenderSideID, bitstream);
		Transmission.PAYLOAD_HASH_FIELD.write(subjectPayloadHash, bitstream);
		TransmissionStore.COLUMN_RECEIVED_AT.writeValue(subjectReceivedAt, bitstream);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException
	{
		subjectSenderSideID = (int) Transmission.TRANSMISSION_ID_FIELD.read(bitstream);
		subjectPayloadHash = (int) Transmission.PAYLOAD_HASH_FIELD.read(bitstream);
		subjectReceivedAt = TransmissionStore.COLUMN_RECEIVED_AT.readValue(bitstream); 
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
	 * @return the subjectReceivedAt
	 */
	public TimeStamp getSubjectReceivedAt()
	{
		return subjectReceivedAt;
	}

}
