package uk.ac.ucl.excites.sapelli.sender;

import java.util.Calendar;

import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class BootReceiver extends BroadcastReceiver
{

	@Override
	public void onReceive(Context mContext, Intent mIntent)
	{

		PendingIntent mPendingIntent = null;

		if(DataSenderPreferences.getSenderEnabled(mContext))
		{
			// TODO Re-enable the service at same point
			// Intent service = new Intent(mContext, DataSenderService.class);
			// mPendingIntent = PendingIntent.getService(mContext, 0, service, 0);
		}

		// Set up a calendar 2 minutes from now
		Calendar mCalendar = Calendar.getInstance();
		mCalendar.setTimeInMillis(System.currentTimeMillis());
		mCalendar.add(Calendar.MINUTE, 2);

		// TODO Re-enable the service at same point
		// Schedule the alarm!
		// AlarmManager mAlarmManager = (AlarmManager) mContext.getSystemService(Context.ALARM_SERVICE);
		// mAlarmManager.set(AlarmManager.RTC_WAKEUP, mCalendar.getTimeInMillis(), mPendingIntent);
	}

}
