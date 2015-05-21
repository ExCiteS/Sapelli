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

package uk.ac.ucl.excites.sapelli.transmission.model;

import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * 
 * @author benelliott
 */
public abstract class Correspondent
{
	
	static public final int CORRESPONDENT_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping CORRESPONDENT_ID_FIELD = IntegerRangeMapping.ForSize(0, CORRESPONDENT_ID_SIZE); // unsigned(!) 24 bit integer
	
	static public final int CORRESPONDENT_NAME_MAX_LENGTH_BYTES = 128;
	static public final int CORRESPONDENT_ADDRESS_MAX_LENGTH_BYTES = 512; // TODO 128 chars? UTF8?
	static public final int CORRESPONDENT_ENCRYPTION_KEY_MAX_LENGTH_BYTES = 32; //TODO 256 bit?
	
	static public interface Handler
	{
		
		public void handle(SMSCorrespondent smsCorrespondent);
		
		// TODO http server/client?
		
	}
	
	private Integer localID;
	private final String name; // name
	private final Transmission.Type transmissionType;
	//private String key; // encryption key TODO ??
	
	/**
	 * @param name
	 * @param transmissionType
	 */
	public Correspondent(String name, Transmission.Type transmissionType)
	{
		if(name == null || name.isEmpty())
			throw new IllegalArgumentException("Please provide a non-empty name String");
		if(name.length() > CORRESPONDENT_NAME_MAX_LENGTH_BYTES)
			throw new IllegalArgumentException("Correspondent name is too long");
		this.name = name;
		this.transmissionType = transmissionType;
	}
	
	/**
	 * @return the localID
	 */
	public int getLocalID()
	{
		return localID;
	}

	public boolean isLocalIDSet()
	{
		return localID != null;
	}
	
	/**
	 * @param localID the localID to set
	 */
	public void setLocalID(int localID)
	{
		this.localID = localID;
	}

	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * @return the transmissionType
	 */
	public Transmission.Type getTransmissionType()
	{
		return transmissionType;
	}

	/**
	 * Return the "address": phone number (for SMS) or URL (for HTTP)
	 * 
	 * @return the address
	 */
	public abstract String getAddress();
	
	public abstract void handle(Handler handle);
	
	@Override
	public String toString()
	{
		return name + " [" + getAddress() + "]";
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof Correspondent)
		{
			Correspondent that = (Correspondent) obj;
			return	this.name.equals(that.name) &&
					this.transmissionType == that.transmissionType &&
					(this.getAddress() != null ? this.getAddress().equals(that.getAddress()) : that.getAddress() == null);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + name.hashCode();
		hash = 31 * hash + transmissionType.ordinal();
		hash = 31 * hash + (getAddress() == null ? 0 : getAddress().hashCode());
		return hash;
	}
	
}
