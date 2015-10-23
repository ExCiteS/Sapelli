package uk.ac.ucl.excites.sapelli.sender.gsm;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.telephony.SmsManager;
import uk.ac.ucl.excites.sapelli.shared.util.android.Debug;

public class SentSMSReceiver extends BroadcastReceiver
{
	public static final String SENT_SMS_RECEIVER_ACTION = "uk.ac.ucl.excites.sapelli.SENT_SMS";
	public static final String DELIVERED_SMS_RECEIVER_ACTION = "uk.ac.ucl.excites.sapelli.DELIVERED_SMS";

	public static final String MESSAGE_ID = "MESSAGE_ID";
	public static final String PART = "PART";
	public static final String NUMBER_OF_PART = "NUMBER_OF_PART";

	@Override
	public void onReceive(Context context, Intent intent)
	{
		// Handle SMS Sent Action
		if(SENT_SMS_RECEIVER_ACTION.equals(intent.getAction()))
		{
			// Get Parameters
			int messageID = intent.getExtras().getInt(MESSAGE_ID);
			int part = intent.getExtras().getInt(PART);
			int numParts = intent.getExtras().getInt(NUMBER_OF_PART);

			// Prepare log message:
			String msgDescription = "[SMS-ID: " + messageID + (numParts > 1 ? ("; SMS-PART:" + part + "/" + numParts) : "]");

			// Handle result:
			switch(getResultCode())
			{
			case Activity.RESULT_OK:
				// TODO msg.sentCallback(); // !!!
				Debug.d("Sending " + msgDescription + ": success.");
				break;
			case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
				Debug.d("Sending " + msgDescription + ": generic failure.");
				break;
			case SmsManager.RESULT_ERROR_NO_SERVICE:
				Debug.d("Sending " + msgDescription + ": no service error.");
				break;
			case SmsManager.RESULT_ERROR_NULL_PDU:
				Debug.d("Sending " + msgDescription + ": null PDU error.");
				break;
			case SmsManager.RESULT_ERROR_RADIO_OFF:
				Debug.d("Sending " + msgDescription + ": radio off error.");
				break;
			}
		}

		// Handle SMS Delivered Action
		if(DELIVERED_SMS_RECEIVER_ACTION.equals(intent.getAction()))
		{
			// Get Parameters
			int messageID = intent.getExtras().getInt(MESSAGE_ID);
			int part = intent.getExtras().getInt(PART);
			int numParts = intent.getExtras().getInt(NUMBER_OF_PART);

			// Prepare log message:
			String msgDescription = "[SMS-ID: " + messageID + (numParts > 1 ? ("; SMS-PART:" + part + "/" + numParts) : "]");

			// Handle result:
			switch(getResultCode())
			{
			case Activity.RESULT_OK:
				// TODO msg.deliveryCallback(); // !!!
				Debug.d("Delivery " + msgDescription + ": success.");
				break;
			case Activity.RESULT_CANCELED:
				Debug.d("Delivery " + msgDescription + ": failure.");
				break;
			}
		}
	}
}