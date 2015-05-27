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

package uk.ac.ucl.excites.sapelli.transmission.model.transport.sms;

import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;

/**
 * @author julia, mstevens
 *
 */
public class SMSCorrespondent extends Correspondent
{
	
	private String phoneNumber;
	
	public SMSCorrespondent(String name, String phoneNumber, boolean binarySMS)
	{
		super(name, binarySMS ? Transmission.Type.BINARY_SMS : Transmission.Type.TEXTUAL_SMS);
		if(phoneNumber == null || phoneNumber.isEmpty() || phoneNumber.length() > CORRESPONDENT_ADDRESS_MAX_LENGTH_CHARS)
			throw new IllegalArgumentException("Invalid phone number.");
		this.phoneNumber = phoneNumber;
	}

	/**
	 * To be called upon database retrieval only
	 * 
	 * @param localID
	 * @param name
	 * @param address
	 * @param binarySMS
	 */
	public SMSCorrespondent(int localID, String name, String address, boolean binarySMS)
	{
		this(name, address, binarySMS); // address = phoneNumber
		setLocalID(localID);
	}
	
	@Override
	public String getAddress()
	{
		return getPhoneNumber();
	}
	
	/**
	 * @return the phoneNumber
	 */
	public String getPhoneNumber()
	{
		return phoneNumber;
	}

	@Override
	public void handle(Handler handle)
	{
		handle.handle(this);
	}

}
