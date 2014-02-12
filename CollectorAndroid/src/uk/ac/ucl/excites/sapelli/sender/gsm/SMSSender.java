package uk.ac.ucl.excites.sapelli.sender.gsm;

import java.util.ArrayList;

import uk.ac.ucl.excites.sapelli.collector.database.DataAccess;
import uk.ac.ucl.excites.sapelli.transmission.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.transmission.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSService;
import uk.ac.ucl.excites.sapelli.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.util.BinaryHelpers;
import android.app.Activity;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.telephony.SmsManager;
import android.util.Log;

public class SMSSender implements SMSService
{
	
	private static final String TAG = "SMSSender";
	private static final String SMS_SENT = "SMS_SENT";
	private static final String SMS_DELIVERED = "SMS_DELIVERED";
	private static final short SMS_PORT = 2013;
	
	private static int MESSAGE_ID = 0;
	
	private Context context;
	private DataAccess dao;
	private SmsManager smsManager;
	
	public SMSSender(Context context, DataAccess dao)
	{
		this.context = context;
		this.dao = dao;
		this.smsManager = SmsManager.getDefault();
	}

	@Override
	public boolean send(final TextMessage textSMS)
	{	
		// Increment message ID!:
		MESSAGE_ID++;
		//Try sending:
		try
		{
			if(textSMS.isMultiPart())
			{	// Send multiple SMSs:
				ArrayList<String> parts = smsManager.divideMessage(textSMS.getText());
				ArrayList<PendingIntent> sentIntents = new ArrayList<PendingIntent>();
				ArrayList<PendingIntent> deliveryIntents = new ArrayList<PendingIntent>();
				for(int p = 0; p < parts.size(); p++)
				{
					sentIntents.add(setupSentCallback(textSMS, MESSAGE_ID, p, parts.size()));
					deliveryIntents.add(setupDeliveredCallback(textSMS, MESSAGE_ID, p, parts.size()));
				}
				smsManager.sendMultipartTextMessage(textSMS.getReceiver().getPhoneNumber(),
													null,
													parts,
													sentIntents,
													deliveryIntents);			
			}
			else
			{	// Send single SMS:	
				smsManager.sendTextMessage(	textSMS.getReceiver().getPhoneNumber(),
											null,
											textSMS.getText(),
											setupSentCallback(textSMS, MESSAGE_ID),
											setupDeliveredCallback(textSMS, MESSAGE_ID));
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon sending " + (textSMS.isMultiPart() ? "multipart " : "")  + "text SMS to " + textSMS.getReceiver().getPhoneNumber());
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
		//Try sending:
		try
		{
			Log.d(TAG, "Sending binary SMS, content hash: " + BinaryHelpers.toHexadecimealString(Hashing.getMD5Hash(binarySMS.getBytes()).toByteArray()));
			smsManager.sendDataMessage(	binarySMS.getReceiver().getPhoneNumber(),
										null,
										SMS_PORT,
										binarySMS.getBytes(),
										setupSentCallback(binarySMS, MESSAGE_ID),
										setupDeliveredCallback(binarySMS, MESSAGE_ID));
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
	
	private PendingIntent setupSentCallback(final Message msg, final int messageID)
	{
		return setupSentCallback(msg, messageID, 1, 1);
	}
	
	private PendingIntent setupSentCallback(final Message msg, final int messageID, final int part, final int numParts)
	{
		// Generate intentAction (to be used by both the PendingIntent and the Receiver):
		String intentAction = SMS_SENT + "_" + messageID + (numParts > 1 ? ("_" + part + "/" + numParts) : "");
		// Set-up receiver:
		context.registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				// Unregister receiver (to avoid it being triggered more than once):
				context.unregisterReceiver(this);
				// Prepare log message:
				String msgDescription = "[SMS-ID: " + messageID +
										(numParts > 1 ? ("; SMS-PART:" + part + "/" + numParts) : "") +
										"; TRANSMISSION-ID: " + msg.getTransmissionID() +
										"; TRANSMISSION-PART: " + msg.getPartNumber() + "/" + msg.getTotalParts() + "]";
				// Handle result:
				switch(getResultCode())
				{
					case Activity.RESULT_OK:
						msg.sentCallback(); //!!!
						dao.store(msg.getTransmission()); //!!! update the transmission
						Log.i(TAG, "Sending " + msgDescription + ": success.");
						break;
					case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
						Log.i(TAG, "Sending " + msgDescription + ": generic failure.");
						break;
					case SmsManager.RESULT_ERROR_NO_SERVICE:
						Log.i(TAG, "Sending " + msgDescription + ": no service error.");
						break;
					case SmsManager.RESULT_ERROR_NULL_PDU:
						Log.i(TAG, "Sending " + msgDescription + ": null PDU error.");
						break;
					case SmsManager.RESULT_ERROR_RADIO_OFF:
						Log.i(TAG, "Sending " + msgDescription + ": radio off error.");
						break;
				}
			}
		},
		new IntentFilter(intentAction));
		//Return pending intent:
		return PendingIntent.getBroadcast(context, 0, new Intent(intentAction), PendingIntent.FLAG_ONE_SHOT);
	}
	
	private PendingIntent setupDeliveredCallback(final Message msg, final int messageID)
	{
		return setupDeliveredCallback(msg, messageID, 1, 1);
	}
	
	private PendingIntent setupDeliveredCallback(final Message msg, final int messageID, final int part, final int numParts)
	{
		// Generate intentAction (to be used by both the PendingIntent and the Receiver):
		String intentAction = SMS_DELIVERED + "_" + messageID + (numParts > 1 ? ("_" + part + "/" + numParts) : "");
		// Set-up receiver:
		context.registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				// Unregister receiver (to avoid it being triggered more than once):
				context.unregisterReceiver(this);
				// Prepare log message:
				String msgDescription = "[SMS-ID: " + messageID +
										(numParts > 1 ? ("; SMS-PART:" + part + "/" + numParts) : "") +
										"; TRANSMISSION-ID: " + msg.getTransmissionID() +
										"; TRANSMISSION-PART: " + msg.getPartNumber() + "/" + msg.getTotalParts() + "]";
				// Handle result:
				switch(getResultCode())
				{
					case Activity.RESULT_OK:
						msg.deliveryCallback(); //!!!
						dao.store(msg.getTransmission()); //!!! update the transmission
						Log.i(TAG, "Delivery " + msgDescription + ": success.");
						break;
					case Activity.RESULT_CANCELED:
						Log.i(TAG, "Delivery " + msgDescription + ": failure.");
						break;
				}
			}
		},
		new IntentFilter(intentAction));
		//Return pending intent:
		return PendingIntent.getBroadcast(context, 0, new Intent(intentAction), PendingIntent.FLAG_ONE_SHOT);
	}

}