/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * This Payload signifies that the transmission to which an incoming {@link ResponsePayload} referred
 * could not be found (likely because it has already been deleted/hidden).
 * 
 * @author mstevens
 */
public class NoSuchTransmissionPayload extends ResponsePayload
{
	
	protected int originalSubjectSendingSideID;

	/**
	 * To be called from sending side (the side sending the reply to a response, i.e. sender of the supposed original transmission)
	 */
	public NoSuchTransmissionPayload(ResponsePayload receivedResponse)
	{
		super(receivedResponse.getTransmission());
		originalSubjectSendingSideID = receivedResponse.subjectSenderSideID;
	}
	
	/**
	 * To be called from receiving side or upon db retrieval.
	 */
	public NoSuchTransmissionPayload()
	{
		super();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException
	{
		super.write(bitstream);
		Transmission.TRANSMISSION_ID_FIELD.write(originalSubjectSendingSideID, bitstream);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException
	{
		super.read(bitstream);
		originalSubjectSendingSideID = Transmission.TRANSMISSION_ID_FIELD.readInt(bitstream);
	}

	/**
	 * @return the originalSubjectSendingSideID
	 */
	public int getOriginalSubjectSendingSideID()
	{
		return originalSubjectSendingSideID;
	}

	@Override
	public int getType()
	{
		return Payload.BuiltinType.NoSuchTransmission.ordinal();
	}

	@Override
	public boolean acknowledgeReception()
	{
		return false;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Payload#handle(uk.ac.ucl.excites.sapelli.transmission.model.Payload.Handler)
	 */
	@Override
	public void handle(Handler handler) throws Exception
	{
		handler.handle(this);
	}
	
}
