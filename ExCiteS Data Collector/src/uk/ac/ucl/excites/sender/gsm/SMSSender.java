package uk.ac.ucl.excites.sender.gsm;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.database.DataAccess;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.crypto.Hashing;
import uk.ac.ucl.excites.transmission.sms.SMSService;
import uk.ac.ucl.excites.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.transmission.sms.text.TextMessage;
import uk.ac.ucl.excites.util.BinaryHelpers;
import uk.ac.ucl.excites.util.Debug;
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
	
	private Context context;
	private DataAccess dao;
	private SmsManager smsManager;

	// Keep the receivers to Lists
	private List<BroadcastReceiver> textMessageSent;
	private List<BroadcastReceiver> textMessageDelivered;
	private List<BroadcastReceiver> binaryMessageSent;
	private List<BroadcastReceiver> binaryMessageDelivered;
	
	public SMSSender(Context context, DataAccess dao)
	{
		this.context = context;
		this.dao = dao;
		this.smsManager = SmsManager.getDefault();
		
		// Initiate the Lists
		textMessageSent = new ArrayList<BroadcastReceiver>();
		textMessageDelivered = new ArrayList<BroadcastReceiver>();
		binaryMessageSent = new ArrayList<BroadcastReceiver>();
		binaryMessageDelivered = new ArrayList<BroadcastReceiver>();

		Debug.d("Created a new SMSSender: " + this.toString());
	}

	@Override
	public boolean send(final TextMessage textSMS)
	{	
		PendingIntent sentPI = PendingIntent.getBroadcast(context, 0, new Intent(SMS_SENT), 0);
		PendingIntent deliveredPI = PendingIntent.getBroadcast(context, 0, new Intent(SMS_DELIVERED), 0);

		// When the SMS has been sent
		BroadcastReceiver newTextMessageSent = new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				if(!textMessageSent.isEmpty())
					context.unregisterReceiver(textMessageSent.remove(0)); // otherwise the sent notification seems to arrive 2-3 times

				switch(getResultCode())
				{
				case Activity.RESULT_OK:
					textSMS.sentCallback(); //!!!
					updateTransmission(textSMS.getTransmission()); //!!!
					Log.i(TAG, "BroadcastReceiver: Binary SMS " + textSMS.getPartNumber() + "/" + textSMS.getTotalParts() + " of transmission with ID "
							+ textSMS.getTransmissionID() + " has been sent.");
					break;
				case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
					Log.i(TAG, "BroadcastReceiver: Generic failure");
					break;
				case SmsManager.RESULT_ERROR_NO_SERVICE:
					Log.i(TAG, "BroadcastReceiver: No service");
					break;
				case SmsManager.RESULT_ERROR_NULL_PDU:
					Log.i(TAG, "BroadcastReceiver: Null PDU");
					break;
				case SmsManager.RESULT_ERROR_RADIO_OFF:
					Log.i(TAG, "BroadcastReceiver: Radio off");
					break;
				}
			}
		};
		context.registerReceiver(newTextMessageSent, new IntentFilter(SMS_SENT));
		textMessageSent.add(newTextMessageSent);

		// When the SMS has been delivered
		BroadcastReceiver newTextMessageDelivered = new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				if(!textMessageDelivered.isEmpty())
					context.unregisterReceiver(textMessageDelivered.remove(0)); // otherwise the sent notification seems to arrive 2-3 times

				switch(getResultCode())
				{
				case Activity.RESULT_OK:
					textSMS.deliveryCallback(); //!!!
					updateTransmission(textSMS.getTransmission()); //!!!
					Log.i(TAG, "BroadcastReceiver: Binary SMS " + textSMS.getPartNumber() + "/" + textSMS.getTotalParts() + " of transmission with ID "
							+ textSMS.getTransmissionID() + " has been delivered.");
					break;
				case Activity.RESULT_CANCELED:
					Log.i(TAG, "BroadcastReceiver: Binary SMS not delivered");
					break;
				}
			}
		};
		context.registerReceiver(newTextMessageDelivered, new IntentFilter(SMS_DELIVERED));
		textMessageDelivered.add(newTextMessageDelivered);
		
		try
		{
			if(textSMS.isMultiPart())
			{
				// Send multiple SMSs
				ArrayList<String> parts = smsManager.divideMessage(textSMS.getText());
				ArrayList<PendingIntent> sentIntents = new ArrayList<PendingIntent>();
				ArrayList<PendingIntent> deliveryIntents = new ArrayList<PendingIntent>();
				for(int i = 0; i < parts.size(); i++)
				{
					sentIntents.add(sentPI);
					deliveryIntents.add(deliveredPI);
					Log.i(TAG, "Parts Loop: " + i);
				}
				smsManager.sendMultipartTextMessage(textSMS.getReceiver().getPhoneNumber(), null, parts, sentIntents, deliveryIntents);			
			}
			else
				smsManager.sendTextMessage(textSMS.getReceiver().getPhoneNumber(), null, textSMS.getText(), sentPI, deliveredPI);
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon sending " + (textSMS.isMultiPart() ? "multipart " : "")  + "text SMS to " + textSMS.getReceiver().getPhoneNumber());
			return false;
		}
		return true;
	}

	@Override
	public boolean send(final BinaryMessage binarySMS)
	{
		PendingIntent sentPI = PendingIntent.getBroadcast(context, 0, new Intent(SMS_SENT), 0);
		PendingIntent deliveredPI = PendingIntent.getBroadcast(context, 0, new Intent(SMS_DELIVERED), 0);
			
		// When the SMS has been sent
		BroadcastReceiver newBinaryMessageSent = new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				if(!binaryMessageSent.isEmpty())
				{
					final BroadcastReceiver remove = binaryMessageSent.remove(0);
					context.unregisterReceiver(remove); // otherwise the sent notification seems to arrive 2-3 times
					Debug.d("Unregistered binaryMessageSent: " + remove.toString());
				}

				switch(getResultCode())
				{
				case Activity.RESULT_OK:
					binarySMS.sentCallback();
					updateTransmission(binarySMS.getTransmission()); //!!!
					Log.i(TAG, "BroadcastReceiver: SMS " + binarySMS.getPartNumber() + "/" + binarySMS.getTotalParts() + " of transmission with ID " + binarySMS.getTransmissionID() + " has been sent.");
					break;
				case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
					Log.i(TAG, "BroadcastReceiver: Generic failure");
					break;
				case SmsManager.RESULT_ERROR_NO_SERVICE:
					Log.i(TAG, "BroadcastReceiver: No service");
					break;
				case SmsManager.RESULT_ERROR_NULL_PDU:
					Log.i(TAG, "BroadcastReceiver: Null PDU");
					break;
				case SmsManager.RESULT_ERROR_RADIO_OFF:
					Log.i(TAG, "BroadcastReceiver: Radio off");
					break;
				}
			}
		};
		context.registerReceiver(newBinaryMessageSent, new IntentFilter(SMS_SENT));
		binaryMessageSent.add(newBinaryMessageSent);
		Debug.d("Registered binaryMessageSent: " + newBinaryMessageSent.toString());

		// When the SMS has been delivered
		BroadcastReceiver newBinaryMessageDelivered = new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				if(!binaryMessageDelivered.isEmpty())
				{
					final BroadcastReceiver remove = binaryMessageDelivered.remove(0);
					context.unregisterReceiver(remove); // otherwise the sent notification seems to arrive 2-3 times
					Debug.d("Unregistered binaryMessageDelivered: " + remove.toString());
				}

				switch(getResultCode())
				{
				case Activity.RESULT_OK:
					binarySMS.deliveryCallback();
					updateTransmission(binarySMS.getTransmission()); //!!!
					Log.i(TAG, "BroadcastReceiver: SMS " + binarySMS.getPartNumber() + "/" + binarySMS.getTotalParts() + " of transmission with ID " + binarySMS.getTransmissionID() + " has been delivered.");
					break;
				case Activity.RESULT_CANCELED:
					Log.i(TAG, "BroadcastReceiver: SMS not delivered");
					break;
				}
			}
		};
		context.registerReceiver(newBinaryMessageDelivered, new IntentFilter(SMS_DELIVERED));
		binaryMessageDelivered.add(newBinaryMessageDelivered);
		Debug.d("Registered binaryMessageDelivered: " + newBinaryMessageDelivered.toString());

		try
		{
			Log.d(TAG, "Sending binary SMS, content hash: " + BinaryHelpers.toHexadecimealString(Hashing.getMD5Hash(binarySMS.getBytes()).toByteArray()));
			smsManager.sendDataMessage(binarySMS.getReceiver().getPhoneNumber(), null, SMS_PORT, binarySMS.getBytes(), sentPI, deliveredPI);
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon sending binary SMS to " + binarySMS.getReceiver().getPhoneNumber());
			return false;
		}
		return true;
	}
	
	private void updateTransmission(Transmission transmission)
	{
		dao.store(transmission);
	}

}