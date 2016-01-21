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
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Payload that is sent to query whether a receiver has a certain {@link Model}.
 * 
 * @author mstevens
 */
public class ModelQueryPayload extends Payload
{

	private long modelID = -1;
	
	/**
	 * Called from sending side.
	 * 
	 * @param modelID
	 */
	public ModelQueryPayload(long modelID)
	{
		this.modelID = modelID;
	}
	
	/**
	 * Called from receiving side or upon db retrieval.
	 */
	public ModelQueryPayload()
	{
		// do nothing
	}
	
	public int getType()
	{
		return Payload.BuiltinType.ModelQuery.ordinal();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException
	{
		Model.MODEL_ID_FIELD.write(modelID, bitstream);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException
	{
		modelID = Model.MODEL_ID_FIELD.readLong(bitstream);
		// Check if we (=receiving end) have this model:
		try
		{
			transmission.getClient().getModel(modelID);
		}
		catch(UnknownModelException ume)
		{
			throw new PayloadDecodeException(this, ume);
		}
	}

	@Override
	public boolean acknowledgeReception()
	{
		return true; // !!!
	}
	
	public long getModelID()
	{
		return modelID;
	}
	
	@Override
	public void handle(Handler handler) throws Exception
	{
		handler.handle(this);
	}
	
}
