package uk.ac.excites.transmission.snd.sms;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.telephony.SmsMessage;

/**
 * @author Michalis Vitos
 * 
 */
public class SmsReceiver extends BroadcastReceiver
{
	@Override
	public void onReceive(Context context, Intent intent)
	{
		// ---get the SMS message passed in---
		Bundle bundle = intent.getExtras();
		SmsMessage[] msgs = null;
		String str = "";
		if (bundle != null)
		{
			// ---retrieve the SMS message received---
			Object[] pdus = (Object[]) bundle.get("pdus");
			msgs = new SmsMessage[pdus.length];
			for (int i = 0; i < msgs.length; i++)
			{
				msgs[i] = SmsMessage.createFromPdu((byte[]) pdus[i]);
				str += "SMS " + (i + 1) + " from " + msgs[i].getOriginatingAddress() + "\n";
				str += "SMS char lenght: " + msgs[i].getMessageBody().toString().length();
				str += " : \n\n";
				str += msgs[i].getMessageBody().toString();
				str += "\n\n";
			}

			Intent mIntent = new Intent(context, SmsShow.class);
			mIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
			mIntent.putExtra("sms", str);
			context.startActivity(mIntent);

			// This will stop the Broadcast and not allow the message to
			// be interpreted by the default Android app or other apps
			abortBroadcast();
		}
	}
}