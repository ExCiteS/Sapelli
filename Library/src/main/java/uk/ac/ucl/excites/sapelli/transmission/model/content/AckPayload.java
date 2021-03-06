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

import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;

/**
 * A Payload used to acknowledge the successful reception (& decoding, processing, etc.) of an other transmission.
 * The receiver of that "subject" transmission will send this payload back to the original sender.
 * 
 * @author mstevens
 */
public class AckPayload extends ResponsePayload
{
	
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
		// no need for a callback
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
	public final boolean acknowledgeReception()
	{
		return false; // !!!
	}

	@Override
	public void handle(Handler handler) throws Exception
	{
		handler.handle(this);
	}

}
