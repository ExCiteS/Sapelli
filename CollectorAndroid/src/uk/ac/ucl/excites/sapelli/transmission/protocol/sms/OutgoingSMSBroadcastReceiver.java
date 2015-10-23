package uk.ac.ucl.excites.sapelli.transmission.protocol.sms;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.telephony.SmsManager;
import uk.ac.ucl.excites.sapelli.shared.util.android.Debug;

/**
 * 
 * @author Michalis Vitos, mstevens
 */
public abstract class OutgoingSMSBroadcastReceiver extends BroadcastReceiver
{
	
	// TODO put this in a resource file so we can also load them from there in the AndroidManifest
	static public final String SENT_SMS_RECEIVER_ACTION = "uk.ac.ucl.excites.sapelli.SENT_SMS";
	static public final String DELIVERED_SMS_RECEIVER_ACTION = "uk.ac.ucl.excites.sapelli.DELIVERED_SMS";

	static public final String MESSAGE_ID = "MESSAGE_ID";
	static public final String PART = "PART";
	static public final String NUMBER_OF_PART = "NUMBER_OF_PART";

	@Override
	public void onReceive(Context context, Intent intent)
	{
		// Get Parameters
		int messageID = intent.getExtras().getInt(MESSAGE_ID);
		int part = intent.getExtras().getInt(PART);
		int numParts = intent.getExtras().getInt(NUMBER_OF_PART);
		
		// Prepare log message:
		String msgDescription = "[SMS-ID: " + messageID + (numParts > 1 ? ("; SMS-PART:" + part + "/" + numParts) : "]");
		
		// Delegate to subclass:
		handle(messageID, part, numParts, msgDescription);
	}
	
	protected abstract void handle(int messageID, int part, int numParts, String msgDescription);
	
	/**
	 * @author mstevens
	 */
	static public class Sent extends OutgoingSMSBroadcastReceiver
	{

		@Override
		protected void handle(int messageID, int part, int numParts, String msgDescription)
		{
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
		
	}
	
	/**
	 * @author mstevens
	 */
	static public class Delivered extends OutgoingSMSBroadcastReceiver
	{

		@Override
		protected void handle(int messageID, int part, int numParts, String msgDescription)
		{
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