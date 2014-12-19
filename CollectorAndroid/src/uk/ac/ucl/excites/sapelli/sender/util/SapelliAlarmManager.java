package uk.ac.ucl.excites.sapelli.sender.util;

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
	public static final String PROJECT_FINGERPRINT = "fingerPrint";

	public SapelliAlarmManager()
	{
		// Nothing goes here
	}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSenderService}, initially after a minute and then every <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param intervalMillis
	 * @param projectID
	 * @param fingerPrint
	 */
	public static void setAlarm(Context context, int intervalMillis, int projectID, int fingerPrint)
	{
		setAlarm(context, 60 * 1000, intervalMillis, projectID, fingerPrint);
	}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSenderService}, initially after <code>triggerAtMillis</code> and then every
	 * <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param triggerDelay
	 * @param intervalMillis
	 * @param projectID
	 * @param fingerPrint
	 */
	public static void setAlarm(Context context, int triggerDelay, int intervalMillis, int projectID, int fingerPrint)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);

		// Setup the alarm to be triggered every intervalMillis
		alarmManager.setRepeating(AlarmManager.ELAPSED_REALTIME_WAKEUP, SystemClock.elapsedRealtime() + triggerDelay, intervalMillis,
				getAlarmIntent(context, projectID, fingerPrint));

		// Enable the Broadcast Receiver
		enableBootReceiver(context, true);
	}


	/**
	 * @param context
	 * @param projectID
	 * @param fingerPrint
	 */
	public static void cancelAlarm(Context context, int projectID, int fingerPrint)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
		alarmManager.cancel(getAlarmIntent(context, projectID, fingerPrint));

		// Check whether to cancel BootReceiver
		checkBootReceiver(context);
	}

	/**
	 * @param context
	 * @param projectID
	 * @param fingerPrint
	 * @return
	 */
	private static PendingIntent getAlarmIntent(Context context, int projectID, int fingerPrint)
	{
		// Create the PendingIntent for the DataSenderService
		Intent serviceIntent = new Intent(context, DataSenderService.class);
		serviceIntent.putExtra(PROJECT_ID, projectID);
		serviceIntent.putExtra(PROJECT_FINGERPRINT, fingerPrint);
		PendingIntent alarmIntent = PendingIntent.getService(context, projectID, serviceIntent, 0);
		return alarmIntent;
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
		ProjectStore projectStore = null; // TODO This will crash, it needs a way of accessing the CollectoApp and get the projectstore
		// ((CollectorApp) context.getApplication()).getProjectStore(null);

		// For each of the projects that has sending enabled, set an Alarm
		for(Project p : projectStore.retrieveProjects())
		{
			// TODO if (p.isSending())
			isSending = false; // TODO change
			break;
		}

		enableBootReceiver(context, isSending);
	}

	/**
	 * @param context
	 * @param enable
	 */
	private static void enableBootReceiver(Context context, boolean enable)
	{
		ComponentName receiver = new ComponentName(context, BootReceiver.class);
		PackageManager pm = context.getPackageManager();

		final int newState = (enable) ? PackageManager.COMPONENT_ENABLED_STATE_ENABLED : PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
		pm.setComponentEnabledSetting(receiver, newState, PackageManager.DONT_KILL_APP);
	}
}
