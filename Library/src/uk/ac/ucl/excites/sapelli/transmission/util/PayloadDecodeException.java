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

package uk.ac.ucl.excites.sapelli.transmission.util;

import uk.ac.ucl.excites.sapelli.transmission.Payload;

/**
 * @author mstevens
 *
 */
public class PayloadDecodeException extends Exception
{

	private static final long serialVersionUID = 1L;
	
	protected Payload payload;
	
	public PayloadDecodeException(Payload payload, String message, Throwable cause)
	{
		super(message, cause);
		this.payload = payload;
	}
	
	public PayloadDecodeException(Payload payload, String message)
	{
		this(payload, message, null);
	}

	/**
	 * @return the payload
	 */
	public Payload getPayload()
	{
		return payload;
	}
	
}
