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
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Payload that is sent to signify that the sender (of this payload) is unfamiliar with the model that the receiver has just sent records for.
 * 
 * This is a ResponsePayload because a model request will be sent as a result of receiving records of an unknown model.
 * 
 * TODO perhaps also include a list of the model IDs that *are* known?
 * 
 * @author benelliott
 */
public class ModelRequestPayload extends ResponsePayload
{

	private long unknownModelID;
	
	public ModelRequestPayload(Transmission<?> subject, long unknownModelID)
	{
		super(subject);
		this.unknownModelID = unknownModelID;
	}
	
	@Override
	public int getType()
	{
		return Payload.BuiltinType.ModelRequest.ordinal();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		super.write(bitstream);
		Model.MODEL_ID_FIELD.write(unknownModelID, bitstream);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		super.read(bitstream);
		unknownModelID = Model.MODEL_ID_FIELD.readLong(bitstream);
	}

	@Override
	public boolean acknowledgeReception()
	{
		return true; // ?
	}
	
	public long getUnknownModelID()
	{
		return unknownModelID;
	}
}
