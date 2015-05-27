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
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * A Payload used to acknowledge the successful reception (& decoding, processing, etc.) of an other transmission.
 * The receiver of that "subject" transmission will send this payload back to the original sender.
 * 
 * @author mstevens
 */
public class AckPayload extends ResponsePayload
{
	
	private TimeStamp subjectReceivedAt;

	/**
	 * To be called from receiving side
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
	public AckPayload(Transmission<?> subject)
	{
		super(subject);
		this.subjectReceivedAt = subject.getReceivedAt();
		// no need for a callback
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.content.ResponsePayload#write(uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream)
	 */
	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException
	{
		super.write(bitstream);
		TransmissionStore.COLUMN_RECEIVED_AT.writeValue(subjectReceivedAt, bitstream);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.content.ResponsePayload#read(uk.ac.ucl.excites.sapelli.shared.io.BitInputStream)
	 */
	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		super.read(bitstream);
		subjectReceivedAt = new TimeStamp(TransmissionStore.COLUMN_RECEIVED_AT.readValue(bitstream).getMsSinceEpoch(), DateTimeZone.getDefault());
		// the TimeStamp read by the column is in UTC so we convert to local timezone (of this device, i.e. the receiver) to match the other timestamps stored with transmissions 
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
	public boolean acknowledgeReception()
	{
		return false; // !!!
	}

	/**
	 * @return the subjectReceivedAt
	 */
	public TimeStamp getSubjectReceivedAt()
	{
		return subjectReceivedAt;
	}

	@Override
	public void handle(Handler handler) throws Exception
	{
		handler.handle(this);
	}

}
