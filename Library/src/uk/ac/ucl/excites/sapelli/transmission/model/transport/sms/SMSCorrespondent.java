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

import com.google.i18n.phonenumbers.NumberParseException;
import com.google.i18n.phonenumbers.PhoneNumberUtil;
import com.google.i18n.phonenumbers.PhoneNumberUtil.PhoneNumberFormat;
import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission.Type;

/**
 * @author julia, mstevens
 *
 */
public class SMSCorrespondent extends Correspondent
{
	
	// STATIC -------------------------------------------------------
	/**
	 * @param phoneNumber
	 * @param defaultCountryISOCode the ISO 3166-1 two-letter region code that denotes the region that we are expecting the number to be from - may be null if phoneNumber starts with '+'
	 * @return
	 * @throws IllegalArgumentException
	 */
	static public PhoneNumber toPhoneNumber(String phoneNumber, String defaultCountryISOCode) throws IllegalArgumentException
	{
		try
		{
			return PhoneNumberUtil.getInstance().parse(phoneNumber, defaultCountryISOCode != null ? defaultCountryISOCode.toUpperCase() : null);
		}
		catch(NumberParseException e)
		{
			throw new IllegalArgumentException("Error parsing phone number", e);
		}
	}
	
	static public int findCountryCode(String countryISOCode)
	{
		if(countryISOCode == null)
			throw new NullPointerException("countryISOCode cannot be null!");
	    return PhoneNumberUtil.getInstance().getCountryCodeForRegion(countryISOCode.toUpperCase());
	}
	
	/**
	 * @param phoneNumberInternational a phone number in international format
	 * @return
	 * @throws IllegalArgumentException
	 */
	static public PhoneNumber toPhoneNumber(String phoneNumberInternational) throws IllegalArgumentException
	{
		return toPhoneNumber(phoneNumberInternational, null);
	}
	
	/**
	 * @param phoneNumber
	 * @return the given phoneNumber as a String in international format
	 */
	static public String formatInternational(PhoneNumber phoneNumber)
	{
		return PhoneNumberUtil.getInstance().format(phoneNumber, PhoneNumberFormat.INTERNATIONAL);
	}
	
	/**
	 * @param phoneNumber
	 * @return the given phoneNumber as a String in E164 ("dialable") format (= international format but without spaces and formatting)
	 */
	static public String formatDialable(PhoneNumber phoneNumber)
	{
		return PhoneNumberUtil.getInstance().format(phoneNumber, PhoneNumberFormat.E164);
	}
	
	/**
	 * @param phoneNumber
	 * @return the given phoneNumber as a String formated as address of a correspondent as stored by the TransmissionStore
	 */
	static public String getAddressString(PhoneNumber phoneNumber) throws IllegalArgumentException
	{
		return formatDialable(phoneNumber);
	}
	
	static final public boolean DEFAULT_BINARY_SMS = true;
	
	// DYNAMIC ------------------------------------------------------
	private final PhoneNumber phoneNumber;
	
	/**
	 * @param name
	 * @param phoneNumber
	 * @param binarySMS
	 */
	public SMSCorrespondent(String name, PhoneNumber phoneNumber, boolean binarySMS)
	{
		super(name, binarySMS ? Type.BINARY_SMS : Type.TEXTUAL_SMS);
		if(phoneNumber == null)
			throw new NullPointerException("Please provide a non-null PhoneNumber instance.");
		this.phoneNumber = phoneNumber;
	}
	
	/**
	 * @param name
	 * @param phoneNumber
	 * @param defaultCountryCode the ISO 3166-1 two-letter region code that denotes the region that we are expecting the number to be from.
	 * @param binarySMS
	 * @throws IllegalArgumentException
	 */
	public SMSCorrespondent(String name, String phoneNumber, String defaultCountryCode, boolean binarySMS) throws IllegalArgumentException
	{
		this(name, toPhoneNumber(phoneNumber, defaultCountryCode), binarySMS);
	}
	
	/**
	 * @param name
	 * @param phoneNumberInternational a phone number in international format
	 * @param binarySMS
	 * @throws Exception
	 */
	public SMSCorrespondent(String name, String phoneNumberInternational, boolean binarySMS) throws IllegalArgumentException
	{
		this(name, toPhoneNumber(phoneNumberInternational), binarySMS);
	}

	/**
	 * To be called upon database retrieval only.
	 * 
	 * @param localID
	 * @param name
	 * @param phoneNumberInternational a phone number in international format
	 * @param binarySMS
	 */
	public SMSCorrespondent(int localID, String name, String phoneNumberInternational, boolean binarySMS) throws IllegalArgumentException
	{
		this(name, phoneNumberInternational, binarySMS);
		setLocalID(localID);
	}
	
	/**
	 * @return the phone number as a String in international format
	 */
	public String getPhoneNumberInternational()
	{
		return formatInternational(phoneNumber);
	}
	
	/**
	 * @return the phone number as a String in E164 ("dialable") format (= international format but without spaces and formatting)
	 */
	public String getPhoneNumberDialable()
	{
		return formatDialable(phoneNumber);
	}
	
	@Override
	public String getAddress()
	{
		return getAddressString(phoneNumber);
	}
	
	public boolean isBinary()
	{
		return getTransmissionType() == Type.BINARY_SMS;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Correspondent#favoursLosslessPayload()
	 */
	@Override
	public boolean favoursLosslessPayload()
	{
		return false; // TODO perhaps we can make this user-configurable at some point. 
	}

	@Override
	public String toString()
	{
		return toString(false);
	}
	
	public String toString(boolean includeBinTxt)
	{
		return getName() + " [" + getAddress() + (includeBinTxt ? "; " + (isBinary() ? "bin" : "txt") : "") + "]";
	}
	
	/**
	 * @return the phoneNumber
	 */
	public PhoneNumber getPhoneNumber()
	{
		return phoneNumber;
	}

	@Override
	public void handle(Handler handle)
	{
		handle.handle(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof SMSCorrespondent)
		{
			SMSCorrespondent that = (SMSCorrespondent) obj;
			return	super.equals(that) && // Correspondent#equals(Object)
					this.phoneNumber.equals(that.phoneNumber);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + phoneNumber.hashCode();
		return hash;
	}

}
