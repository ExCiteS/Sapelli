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

package uk.ac.ucl.excites.sapelli.relay.sms;

import java.text.SimpleDateFormat;
import java.util.Date;

import android.annotation.SuppressLint;

/**
 * An SMS Object to store SMSs
 * 
 * @author Michalis Vitos
 * 
 */
public class SmsObject
{
	private long id;
	private String number;
	private long timestamp;
	private String message;

	public SmsObject()
	{
		// this(null, (Long) null, null);
	}

	public SmsObject(String telephoneNumber, long messageTimestamp, String messageData)
	{
		this.number = telephoneNumber;
		this.timestamp = messageTimestamp;
		this.message = messageData;
	}
	
	public long getId()
	{
		return id;
	}

	public void setId(long id)
	{
		this.id = id;
	}

	public String getTelephoneNumber()
	{
		return number;
	}

	public void setTelephoneNumber(String telephoneNumber)
	{
		this.number = telephoneNumber;
	}

	public long getMessageTimestamp()
	{
		return timestamp;
	}

	public void setMessageTimestamp(long messageTimestamp)
	{
		this.timestamp = messageTimestamp;
	}

	public String getMessageData()
	{
		return message;
	}

	public void setMessageData(String messageData)
	{
		this.message = messageData;
	}

	@SuppressLint("SimpleDateFormat")
	public String toString()
	{
		String timeString = "";
		if (timestamp > 0)
		{
			// Date
			Date dateObj = new Date(timestamp);
			SimpleDateFormat df = new SimpleDateFormat("KK:mm:ss dd-MM-yyyy");
			timeString = df.format(dateObj);
		}

		String printOut = "SMS // ";
		printOut += "id: " + id;
		printOut += " | Telephone: " + number;
		printOut += " | Time: " + timeString;
		return printOut;
	}
}