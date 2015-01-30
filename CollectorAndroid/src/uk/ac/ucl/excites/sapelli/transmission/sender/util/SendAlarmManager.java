package uk.ac.ucl.excites.sapelli.transmission.sender.util;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.remote.SendRecordsSchedule;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.sender.RecordSenderService;
import uk.ac.ucl.excites.sapelli.transmission.sender.SendAlarmBootListener;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.SystemClock;
import android.util.Log;

/**
 * Class which schedules the RecordSenderService to send records from a particular project with initial delays and fixed intervals.
 * 
 * @author Michalis Vitos
 * 
 */
public class SendAlarmManager
{
	private static final String TAG = SendAlarmManager.class.getSimpleName();
	
	public static final String INTENT_KEY_PROJECT_ID = "projectId";
	public static final String INTENT_KEY_PROJECT_FINGERPRINT = "fingerPrint";
	private static final int DEFAULT_DELAY_MILLIS = 60 * 1000; // default delay is 1 minute

	/**
	 * Set up an Alarm for a project that calls the {@link RecordSenderService}, initially after a minute and then every <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param intervalMillis
	 * @param projectID
	 * @param fingerPrint
	 */
	public static void setSendRecordsAlarm(Context context, int intervalMillis, int projectID, int fingerPrint)
	{
		setSendRecordsAlarm(context, DEFAULT_DELAY_MILLIS, intervalMillis, projectID, fingerPrint);
	}

	/**
	 * Set up an Alarm for a project that calls the {@link RecordSenderService}, initially after <code>triggerAtMillis</code> and then every
	 * <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param triggerDelay
	 * @param intervalMillis
	 * @param projectID
	 * @param fingerPrint
	 */
	public static void setSendRecordsAlarm(Context context, int triggerDelay, int intervalMillis, int projectID, int fingerPrint)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);

		// Setup the alarm to be triggered every intervalMillis
		alarmManager.setRepeating(AlarmManager.ELAPSED_REALTIME_WAKEUP, SystemClock.elapsedRealtime() + triggerDelay, intervalMillis,
				getRecordsAlarmIntent(context, projectID, fingerPrint));
		Log.d(TAG, "Just set an alarm for project with ID "+projectID+" to expire every "+intervalMillis+"ms after a delay of "+triggerDelay+"ms.");
		// We know at least one project needs to send, so make sure alarms are re-enabled on boot:
		enableBootReceiver(context, true);
	}


	/**
	 * @param context
	 * @param projectID
	 * @param fingerPrint
	 */
	public static void cancelSendRecordsAlarm(ProjectStore projectStore, TransmissionStore transmissionStore, Context context, int projectID, int fingerPrint)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
		alarmManager.cancel(getRecordsAlarmIntent(context, projectID, fingerPrint));

		// Check whether or not we need to worry about re-enabling alarms on boot:
		checkBootReceiver(projectStore, transmissionStore, context);
	}

	/**
	 * @param context
	 * @param projectID
	 * @param fingerPrint
	 * @return
	 */
	private static PendingIntent getRecordsAlarmIntent(Context context, int projectID, int fingerPrint)
	{
		// Create the PendingIntent for the DataSenderService
		Intent serviceIntent = new Intent(context, RecordSenderService.class);
		serviceIntent.putExtra(INTENT_KEY_PROJECT_ID, projectID);
		serviceIntent.putExtra(INTENT_KEY_PROJECT_FINGERPRINT, fingerPrint);
		PendingIntent alarmIntent = PendingIntent.getService(context, projectID, serviceIntent, 0);
		return alarmIntent;
	}
	
	/**
	 * Checks whether there is any project with Sending activated and either disables or enables the boot receiver
	 * 
	 * @param context
	 */
	private static void checkBootReceiver(ProjectStore projectStore, TransmissionStore transmissionStore, Context context)
	{
		boolean isSending = false;

		// check if any of the projects in the project store need the boot receiver to be enabled:
		for(Project project : projectStore.retrieveProjects()) // TODO projectStore.getSendingProjects?
		{
			SendRecordsSchedule receiver = projectStore.retrieveSendScheduleForProject(project, transmissionStore);
			if (receiver != null)
			{
				// we only need one sending project to know we must enable the receiver
				isSending = true;
				break;
			}
		}

		enableBootReceiver(context, isSending);
	}

	/**
	 * @param context
	 * @param enable
	 */
	private static void enableBootReceiver(Context context, boolean enable)
	{
		ComponentName receiver = new ComponentName(context, SendAlarmBootListener.class);
		PackageManager pm = context.getPackageManager();

		final int newState = (enable) ? PackageManager.COMPONENT_ENABLED_STATE_ENABLED : PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
		pm.setComponentEnabledSetting(receiver, newState, PackageManager.DONT_KILL_APP);
		Log.d(TAG, "Enabled boot receiver");
	}
}
