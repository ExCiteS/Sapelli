package uk.ac.ucl.excites.sender.gsm;

import java.util.ArrayList;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.crypto.Hashing;
import uk.ac.ucl.excites.transmission.sms.SMSService;
import uk.ac.ucl.excites.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.transmission.sms.text.TextMessage;
import uk.ac.ucl.excites.util.BinaryHelpers;
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
	
	public SMSSender(Context context, DataAccess dao)
	{
		this.context = context;
		this.dao = dao;
		this.smsManager = SmsManager.getDefault();
	}

	@Override
	public boolean send(final TextMessage textSMS)
	{	
		PendingIntent sentPI = PendingIntent.getBroadcast(context, 0, new Intent(SMS_SENT), 0);
		PendingIntent deliveredPI = PendingIntent.getBroadcast(context, 0, new Intent(SMS_DELIVERED), 0);

		// When the SMS has been sent
		context.registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				switch(getResultCode())
				{
				case Activity.RESULT_OK:
					textSMS.sentCallback(); //!!!
					updateTransmission(textSMS.getTransmission()); //!!!
					context.unregisterReceiver(this); //otherwise the sent notification seems to arrive 2-3 times
					Log.i(TAG, "BroadcastReceiver: SMS " + textSMS.getPartNumber() + "/" + textSMS.getTotalParts() + " of transmission with ID " + textSMS.getTransmissionID() + " has been sent.");
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
		}, new IntentFilter(SMS_SENT));

		// When the SMS has been delivered
		context.registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				switch(getResultCode())
				{
				case Activity.RESULT_OK:
					textSMS.deliveryCallback(); //!!!
					updateTransmission(textSMS.getTransmission()); //!!!
					context.unregisterReceiver(this); //otherwise the delivery notification seems to arrive 2-3 times
					Log.i(TAG, "BroadcastReceiver: SMS " + textSMS.getPartNumber() + "/" + textSMS.getTotalParts() + " of transmission with ID " + textSMS.getTransmissionID() + " has been delivered.");
					break;
				case Activity.RESULT_CANCELED:
					Log.i(TAG, "BroadcastReceiver: SMS not delivered");
					break;
				}
			}
		}, new IntentFilter(SMS_DELIVERED));
		
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
		context.registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				switch(getResultCode())
				{
				case Activity.RESULT_OK:
					binarySMS.sentCallback();
					updateTransmission(binarySMS.getTransmission()); //!!!
					context.unregisterReceiver(this); //otherwise the sent notification seems to arrive 2-3 times
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
		}, new IntentFilter(SMS_SENT));

		// When the SMS has been delivered
		context.registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				switch(getResultCode())
				{
				case Activity.RESULT_OK:
					binarySMS.deliveryCallback();
					updateTransmission(binarySMS.getTransmission()); //!!!
					context.unregisterReceiver(this); //otherwise the delivery notification seems to arrive 2-3 times
					Log.i(TAG, "BroadcastReceiver: SMS " + binarySMS.getPartNumber() + "/" + binarySMS.getTotalParts() + " of transmission with ID " + binarySMS.getTransmissionID() + " has been delivered.");
					break;
				case Activity.RESULT_CANCELED:
					Log.i(TAG, "BroadcastReceiver: SMS not delivered");
					break;
				}
			}
		}, new IntentFilter(SMS_DELIVERED));

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
		dao.commit();
	}

}