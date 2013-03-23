package uk.ac.ucl.excites.sender.gsm;

import java.util.ArrayList;

import uk.ac.ucl.excites.sender.util.Constants;
import uk.ac.ucl.excites.transmission.sms.SMSService;
import uk.ac.ucl.excites.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.transmission.sms.text.TextMessage;
import uk.ac.ucl.excites.util.DeviceControl;
import android.app.Activity;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.SmsManager;
import android.telephony.TelephonyManager;
import android.util.Log;

public class SMSSender implements SMSService
{
	
	private static final String TAG = "SMSSender";
	private static final String SMS_SENT = "SMS_SENT";
	private static final String SMS_DELIVERED = "SMS_DELIVERED";
	private static final short SMS_PORT = 2013;
	
	private static int serviceState = -1;
	private static int maxAttempts;

	private Context context;
	private TelephonyManager telephonyManager;
	private SmsManager smsManager;
	
	public SMSSender(Context context, int maxAttempts)
	{
		this.context = context;
		this.telephonyManager = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
		this.smsManager = SmsManager.getDefault();
		SMSSender.maxAttempts = maxAttempts;
		setServiceListener();
	}

	public void send()
	{

		signalChecker();

		if(serviceState == ServiceState.STATE_IN_SERVICE)
		{


		}
		
//		// Get in the AirplaneMode only if it is checked in the settings
//		Log.i(Constants.TAG, "The device is" + (AIRPLANE_MODE == true ? "" : " not") + " getting to Airplane Mode.");
//		if(!ServiceChecker.inAirplaneMode(mContext) && (AIRPLANE_MODE == true))
//			ServiceChecker.toggleAirplaneMode(mContext);
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
					Log.i(TAG, "BroadcastReceiver: SMS sent");
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
					Log.i(TAG, "BroadcastReceiver: SMS delivered");
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
					Log.i(TAG, "BroadcastReceiver: SMS sent");
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
					Log.i(TAG, "BroadcastReceiver: SMS delivered");
					break;
				case Activity.RESULT_CANCELED:
					Log.i(TAG, "BroadcastReceiver: SMS not delivered");
					break;
				}
			}
		}, new IntentFilter(SMS_DELIVERED));

		try
		{
			smsManager.sendDataMessage(binarySMS.getReceiver().getPhoneNumber(), null, SMS_PORT, binarySMS.getBytes(), sentPI, deliveredPI);
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon sending binary SMS to " + binarySMS.getReceiver().getPhoneNumber());
			return false;
		}
		return true;
	}

	/**
	 * Check if phone is in Airplane Mode and try to find signal
	 */
	private void signalChecker()
	{
		if(Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "signalCheck() serviceState: " + serviceState);

		// If the phone is in AirplaneMode, set it to off
		// Wait until the phone is connected
		if(DeviceControl.inAirplaneMode(context))
		{
			if(Constants.DEBUG_LOG)
				Log.i(Constants.TAG, "The device is in Airplane mode");

			DeviceControl.toggleAirplaneMode(context);

			int tempCount = 0;

			while(serviceState != ServiceState.STATE_IN_SERVICE && tempCount < maxAttempts)
			{
				Log.i(Constants.TAG, "Connection Attempt! " + tempCount);
				// Wait for 1 a second on every attempt
				try
				{
					Thread.sleep(1000);
				}
				catch(InterruptedException e)
				{
					if(Constants.DEBUG_LOG)
						Log.i(Constants.TAG, "signalCheck() error: " + e.toString());
				}
				tempCount++;
			}
		}
	}

	/**
	 * Check if there is GSM connectivity. The serrviceState has 3 modes, 0 : Normal operation condition, the phone is registered with an operator either in
	 * home network or in roaming. 1 : Phone is not registered with any operator, the phone can be currently searching a new operator to register to, or not
	 * searching to registration at all, or registration is denied, or radio signal is not available. 3 : Radio of telephony is explicitly powered off.
	 */
	private void setServiceListener()
	{
		PhoneStateListener serviceListener = new PhoneStateListener()
		{
			public void onServiceStateChanged(ServiceState service)
			{
				serviceState = service.getState();
				// if(Constants.DEBUG_LOG)
				// Log.i(Constants.TAG, "GSM Service state: " + serviceState);
			}
		};
		telephonyManager.listen(serviceListener, PhoneStateListener.LISTEN_SERVICE_STATE);
	}

}