package uk.ac.ucl.excites.sapelli.sender.util;

import uk.ac.ucl.excites.sapelli.sender.DataSenderService;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;

/**
 * @author Michalis Vitos
 * 
 */
public class SapelliAlarmManager
{
	public static final int SERVICE_REQUEST_CODE = 0;
	public static final String PROJECT_HASH_CODE = "PROJECT_HASH_CODE";

	public SapelliAlarmManager() {}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSenderService}, initially after a minute and then every <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param intervalMillis
	 * @param projectHashCode
	 */
	public static void setAlarm(Context context, int intervalMillis, int projectHashCode)
	{
		setAlarm(context, 60 * 1000, intervalMillis, projectHashCode);
	}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSenderService}, initially after <code>triggerAtMillis</code> and then every
	 * <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param triggerAtMillis
	 * @param intervalMillis
	 * @param projectHashCode
	 */
	public static void setAlarm(Context context, int triggerAtMillis, int intervalMillis, int projectHashCode)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);

		// Setup the alarm to be triggered every intervalMillis
		alarmManager.setInexactRepeating(AlarmManager.ELAPSED_REALTIME_WAKEUP, triggerAtMillis, intervalMillis, getAlarmIntent(context, projectHashCode));
	}

	private static PendingIntent getAlarmIntent(Context context, int projectHashCode)
	{
		// Create the PendingIntent for the DataSenderService
		Intent service = new Intent(context, DataSenderService.class);
		service.putExtra(PROJECT_HASH_CODE, projectHashCode);
		PendingIntent alarmIntent = PendingIntent.getService(context, SERVICE_REQUEST_CODE, service, 0);
		return alarmIntent;
	}

	public static void cancelAlarm(Context context, int projectHashCode)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);

		alarmManager.cancel(getAlarmIntent(context, projectHashCode));
	}
}
