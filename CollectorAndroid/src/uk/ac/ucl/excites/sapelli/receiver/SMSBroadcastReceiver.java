package uk.ac.ucl.excites.sapelli.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.provider.Telephony;
import android.telephony.SmsMessage;
import android.util.Log;
import android.widget.Toast;

public class SMSBroadcastReceiver extends BroadcastReceiver
{
	private static final String TAG = "SMSBroadcastReceiver";
	/**
	 * Called when the BroadcastReceiver hears a broadcast it is interested in (i.e. received SMS).
	 * Will start the SMSReceiverService, passing the relevant message using an Intent.
	 */
	@Override
	public void onReceive(Context context, Intent intent)
	{
		if (intent.getAction().equals(Telephony.Sms.Intents.DATA_SMS_RECEIVED_ACTION))
			Log.d(TAG, "Data SMS received");
		if (intent.getAction().equals(Telephony.Sms.Intents.SMS_RECEIVED_ACTION))
			Log.d(TAG, "Text SMS received");
		Log.d(TAG,"SMS received!");
		Bundle bundle = intent.getExtras();
		SmsMessage[] msgs = null;
		String str = "";
		if(bundle != null)
		{
			// ---retrieve the SMS message received---
			Object[] pdus = (Object[]) bundle.get("pdus");
			msgs = new SmsMessage[pdus.length];
			for(int i = 0; i < msgs.length; i++)
			{
				msgs[i] = SmsMessage.createFromPdu((byte[]) pdus[i]);
				str += "SMS from " + msgs[i].getOriginatingAddress();
				str += " :";
				str += msgs[i].getMessageBody().toString();
				str += "\n";
			}
			// ---display the new SMS message---
			Toast.makeText(context, str, Toast.LENGTH_SHORT).show();
		}
	}
	
	private void dataSMSReceived()
	{
		
	}
	
	private void textSMSReceived()
	{
		
	}

}
