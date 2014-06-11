package uk.ac.ucl.excites.sapelli.sender.gsm;

import java.util.ArrayList;

import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;
import uk.ac.ucl.excites.sapelli.transmission.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSService;
import uk.ac.ucl.excites.sapelli.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.sms.text.TextMessage;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.telephony.SmsManager;
import android.util.Log;

public class SMSSender implements SMSService
{
	private static final String TAG = "SMSSender";

	/**
	 * The choice of port might affect the size of the UDH header Android uses (5 or 7 bytes). <br/>
	 * The current value ({@value #SMS_PORT}) would require 16 bits, and therefore a 7-byte UDH, but it could be that Android always uses the 7 byte header,
	 * even for 8-bit ports. <br/>
	 * <br/>
	 * <b>TODO</b> test with 8-bit port, possibly we would gain bytes of usable content in every {@link BinaryMessage}.
	 * 
	 * @see BinaryMessage#MAX_TOTAL_SIZE_BYTES
	 * @see BinaryMessage
	 * @see <a href="http://en.wikipedia.org/wiki/User_Data_Header">User Data Header (UDH)</a>
	 */
	public static final short SMS_PORT = 2013;

	private static int MESSAGE_ID = 0;

	private Context context;
	private SmsManager smsManager;

	public SMSSender(Context context)
	{
		this.context = context;
		this.smsManager = SmsManager.getDefault();
	}

	@Override
	public boolean send(final TextMessage textSMS)
	{
		// Increment message ID!:
		MESSAGE_ID++;
		// Try sending:
		try
		{
			if(textSMS.isMultiPart())
			{ // Send multiple SMSs:
				ArrayList<String> parts = smsManager.divideMessage(textSMS.getContent());
				ArrayList<PendingIntent> sentIntents = new ArrayList<PendingIntent>();
				ArrayList<PendingIntent> deliveryIntents = new ArrayList<PendingIntent>();
				for(int p = 0; p < parts.size(); p++)
				{
					sentIntents.add(setupSentCallback(MESSAGE_ID, p, parts.size()));
					deliveryIntents.add(setupDeliveredCallback(MESSAGE_ID, p, parts.size()));
				}
				smsManager.sendMultipartTextMessage(textSMS.getReceiver().getPhoneNumber(), null, parts, sentIntents, deliveryIntents);
			}
			else
			{ // Send single SMS:
				smsManager.sendTextMessage(textSMS.getReceiver().getPhoneNumber(), null, textSMS.getContent(), setupSentCallback(MESSAGE_ID),
						setupDeliveredCallback(MESSAGE_ID));
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon sending " + (textSMS.isMultiPart() ? "multipart " : "") + "text SMS to " + textSMS.getReceiver().getPhoneNumber());
			// Failure:
			return false;
		}
		// Success:
		return true;
	}

	public boolean send(final String demosSMS, String phoneNumber)
	{
		// Increment message ID!:
		MESSAGE_ID++;
		// Try sending:
		try
		{
			smsManager.sendTextMessage(phoneNumber, null, demosSMS, setupSentCallback(MESSAGE_ID), setupDeliveredCallback(MESSAGE_ID));
		}
		catch(Exception e)
		{
			Log.e(TAG, e.toString());
			// Failure:
			return false;
		}
		// Success:
		return true;
	}

	@Override
	public boolean send(final BinaryMessage binarySMS)
	{
		// Increment message ID!:
		MESSAGE_ID++;
		// Try sending:
		try
		{
			Log.d(TAG, "Sending binary SMS, content hash: " + BinaryHelpers.toHexadecimealString(Hashing.getMD5Hash(binarySMS.getBytes()).toByteArray()));
			smsManager.sendDataMessage(binarySMS.getReceiver().getPhoneNumber(), null, SMS_PORT, binarySMS.getBytes(), setupSentCallback(MESSAGE_ID),
					setupDeliveredCallback(MESSAGE_ID));
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon sending binary SMS to " + binarySMS.getReceiver().getPhoneNumber());
			// Failure:
			return false;
		}
		// Success:
		return true;
	}

	private PendingIntent setupSentCallback(final int messageID)
	{
		return setupSentCallback(messageID, 1, 1);
	}

	private PendingIntent setupSentCallback(final int messageID, final int part, final int numParts)
	{
		// Create intent
		Intent intent = new Intent(SentSMSReceiver.SENT_SMS_RECEIVER_ACTION);
		intent.putExtra(SentSMSReceiver.MESSAGE_ID, messageID);
		intent.putExtra(SentSMSReceiver.PART, part);
		intent.putExtra(SentSMSReceiver.NUMBER_OF_PART, numParts);

		// Return pending intent:
		return PendingIntent.getBroadcast(context, 0, intent, PendingIntent.FLAG_ONE_SHOT);
	}

	private PendingIntent setupDeliveredCallback(final int messageID)
	{
		return setupDeliveredCallback(messageID, 1, 1);
	}

	private PendingIntent setupDeliveredCallback(final int messageID, final int part, final int numParts)
	{
		// Create intent
		Intent intent = new Intent(SentSMSReceiver.DELIVERED_SMS_RECEIVER_ACTION);
		intent.putExtra(SentSMSReceiver.MESSAGE_ID, messageID);
		intent.putExtra(SentSMSReceiver.PART, part);
		intent.putExtra(SentSMSReceiver.NUMBER_OF_PART, numParts);

		// Return pending intent:
		return PendingIntent.getBroadcast(context, 0, intent, PendingIntent.FLAG_ONE_SHOT);
	}

}