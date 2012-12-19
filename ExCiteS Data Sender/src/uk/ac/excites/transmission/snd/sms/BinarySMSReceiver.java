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
public class BinarySMSReceiver extends BroadcastReceiver
{
	@Override
	public void onReceive(Context context, Intent intent)
	{
		Bundle bundle = intent.getExtras();
		SmsMessage[] msgs = null;

		if (null != bundle)
		{
			String info = "Binary SMS from ";
			Object[] pdus = (Object[]) bundle.get("pdus");
			msgs = new SmsMessage[pdus.length];
			byte[] data = null;

			for (int i = 0; i < msgs.length; i++)
			{
				msgs[i] = SmsMessage.createFromPdu((byte[]) pdus[i]);
				info += msgs[i].getOriginatingAddress();
				info += "\n\n*****BINARY MESSAGE*****\n\n";

				data = msgs[i].getUserData();

				for (int index = 0; index < data.length; ++index)
				{
					info += Character.toString((char) data[index]);
				}
			}

			Intent mIntent = new Intent(context, SmsShow.class);
			mIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
			mIntent.putExtra("sms", info);
			context.startActivity(mIntent);

			// This will stop the Broadcast and not allow the message to
			// be interpreted by the default Android app or other apps
			abortBroadcast();
		}
	}
}
