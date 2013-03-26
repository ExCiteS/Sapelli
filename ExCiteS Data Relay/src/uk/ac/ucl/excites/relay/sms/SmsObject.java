package uk.ac.ucl.excites.relay.sms;

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