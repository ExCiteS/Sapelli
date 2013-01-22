package uk.ac.excites.transmission.relay.sms;

import android.annotation.SuppressLint;
import java.text.SimpleDateFormat;
import java.util.Date;

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
	private byte[] message;

	public SmsObject()
	{
		// this(null, (Long) null, null);
	}

	public SmsObject(String telephoneNumber, long messageTimestamp, byte[] messageData)
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

	public byte[] getMessageData()
	{
		return message;
	}

	public void setMessageData(byte[] messageData)
	{
		this.message = messageData;
	}

	@SuppressLint("SimpleDateFormat")
	public String toString()
	{
		String messageString = "";
		if (message != null)
		{
			for (int index = 0; index < message.length; ++index)
			{
				messageString += Character.toString((char) message[index]);
			}
		}

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
		printOut += " | Message: " + messageString;
		return printOut;
	}
}