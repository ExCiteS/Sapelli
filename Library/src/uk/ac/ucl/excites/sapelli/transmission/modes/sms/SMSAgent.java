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

package uk.ac.ucl.excites.sapelli.transmission.modes.sms;


/**
 * @author julia, mstevens
 *
 */
public class SMSAgent
{
	
	//Statics
	static final char SEPARATOR = ';';
	
	static public SMSAgent Parse(String str)
	{
		String[] parts = str.split("\\" + SEPARATOR, -1); // -1: allow empty Strings
		return new SMSAgent(parts[0]);
	}
	
	//Dynamics
	private String phoneNumber;
	
	public SMSAgent(String phoneNumber)
	{
		if(phoneNumber == null || phoneNumber.isEmpty())
			throw new IllegalArgumentException("Invalid phone number.");
		this.phoneNumber = phoneNumber;
	}
	
	/**
	 * @return the phoneNumber
	 */
	public String getPhoneNumber()
	{
		return phoneNumber;
	}
	
	@Override
	public int hashCode()
	{
		return phoneNumber.hashCode();
	}
	
	@Override
	public boolean equals(Object o)
	{
		if(o instanceof SMSAgent)
			return phoneNumber.equals(((SMSAgent) o).phoneNumber);
		else
			return false;
	}
	
	@Override
	public String toString()
	{
		return phoneNumber + SEPARATOR;
	}

}
