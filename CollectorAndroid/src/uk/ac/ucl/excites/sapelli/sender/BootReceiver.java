package uk.ac.ucl.excites.sapelli.sender;

import uk.ac.ucl.excites.sapelli.sender.util.AlarmScheduler;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class BootReceiver extends BroadcastReceiver
{
	@Override
	public void onReceive(Context context, Intent intent)
	{
		// Start AlarmSchduler service to schedule alarms for the required projects
		Intent alarmScheduler = new Intent(context, AlarmScheduler.class);
		context.startService(alarmScheduler);
	}
}