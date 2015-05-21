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

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
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
	
	protected int subjectSenderSideID;
	protected int subjectPayloadHash;
	protected Integer subjectReceiverSideID;
	
	/**
	 * To be called from receiving side
	 */
	public ResponsePayload()
	{
		super();
	}
	
	/**
	 * To be called from sending side (the side sending the response, = the receiver of the subject)
	 * 
	 */
	public ResponsePayload(Transmission<?> subject)
	{
		super();
		this.subjectSenderSideID = subject.getRemoteID();
		this.subjectPayloadHash = subject.getPayloadHash();
		this.subjectReceiverSideID = subject.isLocalIDSet() ? subject.getLocalID() : null;
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException
	{
		Transmission.TRANSMISSION_ID_FIELD.write(subjectSenderSideID, bitstream);
		Transmission.PAYLOAD_HASH_FIELD.write(subjectPayloadHash, bitstream);
		if(bitstream.write(subjectReceiverSideID != null)) // write presence bit for subjectReceiverSideID
			Transmission.TRANSMISSION_ID_FIELD.write(subjectReceiverSideID, bitstream);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		subjectSenderSideID = Transmission.TRANSMISSION_ID_FIELD.readInt(bitstream);
		subjectPayloadHash = Transmission.PAYLOAD_HASH_FIELD.readInt(bitstream);
		subjectReceiverSideID = bitstream.readBit() ? Transmission.TRANSMISSION_ID_FIELD.readInt(bitstream) : null; // read presence bit, and value if presence = 1/true
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
	 * @return the subjectReceiverSideID
	 */
	public Integer getSubjectReceiverSideID()
	{
		return subjectReceiverSideID;
	}
	
}
