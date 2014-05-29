package uk.ac.ucl.excites.sapelli.sender.util;

import uk.ac.ucl.excites.sapelli.collector.db.PrefProjectStore;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.sender.BootReceiver;
import uk.ac.ucl.excites.sapelli.sender.DataSenderService;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.SystemClock;

/**
 * @author Michalis Vitos
 * 
 */
public class SapelliAlarmManager
{
	public static final String PROJECT_ID = "projectId";

	public SapelliAlarmManager()
	{
	}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSenderService}, initially after a minute and then every <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param intervalMillis
	 * @param projectHashCode
	 */
	public static void setAlarm(Context context, int intervalMillis, int projectID)
	{
		setAlarm(context, 60 * 1000, intervalMillis, projectID);
	}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSenderService}, initially after <code>triggerAtMillis</code> and then every
	 * <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param triggerDelay
	 * @param intervalMillis
	 * @param projectHashCode
	 */
	public static void setAlarm(Context context, int triggerDelay, int intervalMillis, int projectID)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);

		// Setup the alarm to be triggered every intervalMillis
		alarmManager.setRepeating(AlarmManager.ELAPSED_REALTIME_WAKEUP, SystemClock.elapsedRealtime() + triggerDelay, intervalMillis,
				getAlarmIntent(context, projectID));

		// Enable the Broadcast Receiver
		enableBootReceiver(context, true);
	}

	private static PendingIntent getAlarmIntent(Context context, int projectID)
	{
		// Create the PendingIntent for the DataSenderService
		Intent serviceIntent = new Intent(context, DataSenderService.class);
		serviceIntent.putExtra(PROJECT_ID, projectID);
		PendingIntent alarmIntent = PendingIntent.getService(context, projectID, serviceIntent, 0);
		return alarmIntent;
	}

	public static void cancelAlarm(Context context, int projectID)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
		alarmManager.cancel(getAlarmIntent(context, projectID));

		// Check whether to cancel BootReceiver
		checkBootReceiver(context);
	}

	/**
	 * Checks whether there is any project with Sending activated and either disables or enables the boot receiver
	 * 
	 * @param context
	 */
	private static void checkBootReceiver(Context context)
	{
		boolean isSending = false;

		// Get ProjectStore instance:
		ProjectStore projectStore = new PrefProjectStore(context);

		// For each of the projects that has sending enabled, set an Alarm
		for(Project p : projectStore.retrieveProjects())
		{
			// TODO if (p.isSending())
			isSending = false; // TODO change
			break;
		}

		enableBootReceiver(context, isSending);
	}

	private static void enableBootReceiver(Context context, boolean enable)
	{
		ComponentName receiver = new ComponentName(context, BootReceiver.class);
		PackageManager pm = context.getPackageManager();

		final int newState = (enable) ? PackageManager.COMPONENT_ENABLED_STATE_ENABLED : PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
		pm.setComponentEnabledSetting(receiver, newState, PackageManager.DONT_KILL_APP);
	}
}
