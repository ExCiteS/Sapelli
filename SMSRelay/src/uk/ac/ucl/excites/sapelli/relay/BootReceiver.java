package uk.ac.ucl.excites.sapelli.relay;

import java.util.Calendar;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

/**
 * A Boot Receiver that auto starts the Relay on device boot
 * 
 * @author Michalis Vitos
 * 
 */
public class BootReceiver extends BroadcastReceiver
{

	@Override
	public void onReceive(Context context, Intent intent)
	{

		Intent service = new Intent(context, BackgroundService.class);
		PendingIntent pendingIntent = PendingIntent.getService(context, 0, service, 0);

		// Set up a calendar 2 minutes from now
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeInMillis(System.currentTimeMillis());
		calendar.add(Calendar.MINUTE, 2);

		// Schedule the alarm!
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
		alarmManager.set(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(), pendingIntent);
	}

}
