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

import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyServer;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * 
 * @author benelliott, mstevens
 */
public abstract class Correspondent
{
	
	// STATIC -----------------------------------------------------------------
	static public final String UNKNOWN_SENDER_NAME = "anonymous_sender";
	
	static public final int CORRESPONDENT_NAME_MAX_LENGTH_CHARS = 128;
	static public final int CORRESPONDENT_ADDRESS_MAX_LENGTH_CHARS = 2048;
	static public final int CORRESPONDENT_ENCRYPTION_KEY_MAX_LENGTH_BYTES = 32;
	
	static public interface Handler
	{
		
		public void handle(SMSCorrespondent smsCorrespondent);
		
		// TODO http server/client?
		
		public void handle(GeoKeyServer geokeyAccount);
		
	}

	// DYNAMIC ----------------------------------------------------------------
	private Integer localID;
	private String name;
	private final Transmission.Type transmissionType;
	//private String key; TODO ??
	// encryption key TODO ??
	
	/**
	 * If {@code true} the correspondent is marked as deleted by the user.
	 * This way it won't show up in the transmission config UIs, but is
	 * still kept around in the db such that old transmissions can still point to it. 
	 */
	private boolean userDeleted = false;
	
	/**
	 * @param localID may be {@code null} if Correspondent has never been stored
	 * @param name
	 * @param transmissionType
	 */
	public Correspondent(Integer localID, String name, Transmission.Type transmissionType)
	{
		this.localID = localID;
		setName(name);
		this.transmissionType = transmissionType;
	}
	
	public boolean isLocalIDSet()
	{
		return localID != null;
	}
	
	/**
	 * @return
	 * @throws IllegalStateException when no local ID has been set
	 */
	public int getLocalID() throws IllegalStateException
	{
		if(localID == null)
			throw new IllegalStateException("LocalID has not been set yet");
		return localID.intValue();
	}
	
	/**
	 * @param localID the localID to set
	 * @throws IllegalStateException when the Transmission already has a localID which is different from the given one
	 */
	public void setLocalID(int localID) throws IllegalStateException
	{
		if(this.localID != null && this.localID.intValue() != localID)
			throw new IllegalStateException("A different localID value has already been set (existing: " + this.localID + "; new: " + localID + ")!");
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
	 * @param name the name to set
	 */
	public void setName(String name)
	{
		if(name == null || name.isEmpty())
			throw new IllegalArgumentException("Please provide a non-empty name String");
		if(name.length() > CORRESPONDENT_NAME_MAX_LENGTH_CHARS)
			throw new IllegalArgumentException("Correspondent name is too long");
		this.name = name;
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
	
	/**
	 * @return whether or not this Correspondent (and transmission medium) favours losslessly encoded payloads over lossyly encoded ones.
	 */
	public abstract boolean favoursLosslessPayload();

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
		hash = 31 * hash + (userDeleted ? 0 : 1);
		return hash;
	}
	
	public abstract boolean canBeSwappedWithoutNewModelQuery(Correspondent another);
	
}
