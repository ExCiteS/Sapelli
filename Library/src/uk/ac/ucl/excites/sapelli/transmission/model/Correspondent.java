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

import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * 
 * @author benelliott, mstevens
 */
public abstract class Correspondent
{
	
	static public final String UNKNOWN_SENDER_NAME = "anonymous_sender";
	
	static public final boolean DEFAULT_USER_DELETED = false;
	
	static public final int CORRESPONDENT_NAME_MAX_LENGTH_CHARS = 128;
	static public final int CORRESPONDENT_ADDRESS_MAX_LENGTH_CHARS = 512;
	static public final int CORRESPONDENT_ENCRYPTION_KEY_MAX_LENGTH_BYTES = 32;
	
	static public interface Handler
	{
		
		public void handle(SMSCorrespondent smsCorrespondent);
		
		// TODO http server/client?
		
	}
	
	private Integer localID;
	private final String name;
	private final Transmission.Type transmissionType;
	//private String key; TODO ??
	// encryption key TODO ??
	
	/**
	 * If {@code true} the correspondent is marked as deleted by the user.
	 * This way it won't show up in the transmssion config UIs, but is
	 * still kept around in the db such that old transmissions can still point to it. 
	 */
	private boolean userDeleted = DEFAULT_USER_DELETED;
	
	/**
	 * @param name
	 * @param transmissionType
	 */
	public Correspondent(String name, Transmission.Type transmissionType)
	{
		if(name == null || name.isEmpty())
			throw new IllegalArgumentException("Please provide a non-empty name String");
		if(name.length() > CORRESPONDENT_NAME_MAX_LENGTH_CHARS)
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
	
	public abstract void handle(Handler handler);
	
	@Override
	public String toString()
	{
		return name + " [" + getAddress() + "]";
	}
	
	/**
	 * @return the userDeleted
	 */
	public boolean isUserDeleted()
	{
		return userDeleted;
	}

	/**
	 * Mark the correspondent as deleted by the (local) user
	 */
	public void markAsUserDeleted()
	{
		this.userDeleted = true;
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
					(this.getAddress() != null ? this.getAddress().equals(that.getAddress()) : that.getAddress() == null) &&
					this.userDeleted == that.userDeleted;
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
		hash = 31 * hash + (userDeleted ? 0 : 1);
		return hash;
	}
	
}
