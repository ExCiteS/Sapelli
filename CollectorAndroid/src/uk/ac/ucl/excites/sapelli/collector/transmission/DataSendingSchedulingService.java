/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.transmission;

import java.util.Collections;
import java.util.List;

import android.app.AlarmManager;
import android.app.IntentService;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.SystemClock;
import android.util.Log;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * Simple Service for scheduling alarms for projects that need transmission (activated on device boot).
 * 
 * @author Michalis Vitos, mstevens, benelliott
 *
 */
public class DataSendingSchedulingService extends IntentService implements StoreHandle.StoreUser
{

	// STATIC -------------------------------------------------------
	private static final String TAG = DataSendingSchedulingService.class.getSimpleName();

	private static final int DEFAULT_DELAY_MILLIS = 60 * 1000; // default delay is 1 minute

	public static final String INTENT_KEY_PROJECT_ID = "projectId";
	public static final String INTENT_KEY_PROJECT_FINGERPRINT = "fingerPrint";
	
	/**
	 * Start the service with as task the scheduling of all projects that need sending
	 * 
	 * @param context
	 */
	public static void ScheduleAll(Context context)
	{
		context.startService(new Intent(context, DataSendingSchedulingService.class));
	}
	
	public static void Schedule(Context context, Project project)
	{
		Intent intent = new Intent(context, DataSendingSchedulingService.class);
		intent.putExtra(INTENT_KEY_PROJECT_ID, project.getID());
		intent.putExtra(INTENT_KEY_PROJECT_FINGERPRINT, project.getFingerPrint());
		context.startService(intent);
	}
	
	/**
	 * Cancel scheduled sending of data associated with the given project.
	 * 
	 * @param context
	 * @param project
	 */
	public static void Cancel(Context context, Project project)
	{
		Cancel(context, (AlarmManager) context.getSystemService(Context.ALARM_SERVICE), project);
	}
	
	/**
	 * @param context
	 * @param intent
	 */
	public static void Cancel(Context context, PendingIntent intent)
	{
		((AlarmManager) context.getSystemService(Context.ALARM_SERVICE)).cancel(intent);
	}
	
	/**
	 * Cancel scheduled sending of data associated with the project with the given ID & finger print.
	 * 
	 * @param context
	 * @param alarmManager
	 * @param project
	 */
	private static void Cancel(Context context, AlarmManager alarmManager, Project project)
	{
		try
		{
			alarmManager.cancel(GetDataSendingIntent(context, project.getID(), project.getFingerPrint(), null));
		}
		catch(Exception e)
		{
			Log.e(TAG, "Exception upon cancelling alarms", e);
		}
	}
	
	/**
	 * @param projectID
	 * @param projectFingerPrint
	 * @return PendingIntent to start DataSendingService to send data for the given project
	 */
	private static PendingIntent GetDataSendingIntent(Context context, int projectID, int projectFingerPrint, Integer sendScheduleId)
	{
		// Create the PendingIntent for the DataSenderService:
		Intent serviceIntent = new Intent(context, DataSendingService.class);
		// Action (matched by Intent.filterEquals(Intent)):
		serviceIntent.setAction(DataSendingService.getIntentAction(projectID, projectFingerPrint));
		// Extras (not matched by Intent.filterEquals(Intent)):
		if(sendScheduleId != null)
			serviceIntent.putExtra(DataSendingService.INTENT_KEY_SEND_SCHEDULE_ID, sendScheduleId);
		return PendingIntent.getService(context, projectFingerPrint, serviceIntent, 0);
		// Note: we use the project fingerprint as requestCode because it is much less likely to clash than the project id
	}
	
	// DYNAMIC ------------------------------------------------------
	private AlarmManager alarmManager;
	private CollectorApp app;

	/**
	 * A constructor is required, and must call the super IntentService(String) constructor with a name for the worker thread.
	 */
	public DataSendingSchedulingService()
	{
		super(DataSendingSchedulingService.class.getSimpleName());
	}

	/* (non-Javadoc)
	 * @see android.app.IntentService#onStart(android.content.Intent, int)
	 */
	@Override
	public void onStart(Intent intent, int startId)
	{
		Log.d(DataSendingSchedulingService.class.getSimpleName(), "Starting alarm scheduler...");
		super.onStart(intent, startId);
	}

	/**
	 * The IntentService calls this method from the default worker thread with the intent that started the service.
	 * When this method returns, IntentService stops the service, as appropriate.
	 */
	@Override
	protected void onHandleIntent(Intent intent)
	{
		// Get app & alarm manager (do not call this in the constructor!):
		app = ((CollectorApp) getApplication());
		alarmManager = (AlarmManager) getSystemService(Context.ALARM_SERVICE);

		// Read intent data:
		int projectID = intent.getIntExtra(INTENT_KEY_PROJECT_ID, -1);
		int projectFingerPrint = intent.getIntExtra(INTENT_KEY_PROJECT_FINGERPRINT, -1);
		
		ProjectStore projectStore;
		TransmissionStore transmissionStore;
		try
		{
			// Get ProjectStore instance:
			projectStore = app.collectorClient.projectStoreHandle.getStore(this);
			// Get SentTransmissionStore instance:
			transmissionStore = app.collectorClient.transmissionStoreHandle.getStore(this);

			// Projects to schedule for...
			List<Project> projects = (projectID != -1 ?
										// Single project:
										Collections.singletonList(projectStore.retrieveProject(projectID, projectFingerPrint)) :
										// All projects:
										projectStore.retrieveProjects());
			
			Log.d(TAG, "Setting data sending alarms for " + projects.size() + " projects...");
	
			// Set an Alarms for each enabled sending schedule:
			boolean atLeastOne = false;
			for(Project project : projects)
			{
				// Cancel any previously set alarms:
				Cancel(app, alarmManager, project);
				// Schedule for each schedule:
				for(SendSchedule sendSchedule : projectStore.retrieveSendSchedulesForProject(project, transmissionStore))
					atLeastOne = scheduleSending(sendSchedule) | atLeastOne;
			}
	
			// If we have at least one project needs to send, so make sure alarms are re-enabled on boot (if not boot receiver is disabled):
			setupBootReceiver(atLeastOne);
		}
		catch(Exception e)
		{
			Log.d(TAG, "Exception while looking for projects that need alarms", e);
		}
		finally
		{
			app.collectorClient.projectStoreHandle.doneUsing(this);
			app.collectorClient.transmissionStoreHandle.doneUsing(this);
		}
	}
	
	/**
	 * Set up an Alarm for a project that calls the {@link DataSendingService}, initially after a minute and then every <code>intervalMillis</code>
	 * 
	 * @param sendSchedule
	 * @return whether or not the alarm was scheduled
	 */
	private boolean scheduleSending(SendSchedule sendSchedule)
	{
		return scheduleSending(DEFAULT_DELAY_MILLIS, sendSchedule);
	}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSendingService}, initially after <code>triggerAtMillis</code> and then every
	 * <code>intervalMillis</code>
	 * 
	 * @param triggerDelay
	 * @param sendSchedule
	 * @return whether or not the alarm was scheduled
	 */
	private boolean scheduleSending(int triggerDelay, SendSchedule sendSchedule)
	{
		if(!sendSchedule.isEnabled())
			return false;
		
		// Setup the alarm to be triggered every intervalMillis:
		try
		{
			int intervalMillis = sendSchedule.getTransmitIntervalS() * 1000;
			alarmManager.setRepeating(
					AlarmManager.ELAPSED_REALTIME_WAKEUP, // TODO should we really be waking up the device for this?
					SystemClock.elapsedRealtime() + triggerDelay,
					intervalMillis,
					GetDataSendingIntent(app, sendSchedule.getProject().getID(), sendSchedule.getProject().getFingerPrint(), sendSchedule.getID()));
				
				Log.d(TAG, "Set sending alarm for project \"" + sendSchedule.getProject().toString(false) + "\", and receiver \"" + sendSchedule.getReceiver().toString() + "\" to expire every " + intervalMillis + "ms after a delay of " + triggerDelay + "ms.");
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon scheduling alarm", e);
			return false;
		}
		
		return true;
	}

	/**
	 * @param enabled
	 */
	private void setupBootReceiver(boolean enabled)
	{
		ComponentName receiver = new ComponentName(app, BootListener.class);
		int newState = (enabled) ? PackageManager.COMPONENT_ENABLED_STATE_ENABLED : PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
		app.getPackageManager().setComponentEnabledSetting(receiver, newState, PackageManager.DONT_KILL_APP);
		Log.d(TAG, (enabled ? "En" : "Dis") + "abled " + BootListener.class.getSimpleName());
	}

	/**
	 * BroadcastReceiver that listens for device boot events and when one is received, starts the SendAlarmInitialiser service.
	 * 
	 * Note that this BroadcastReceiver is only registered to listen for boot events if Sapelli determines that there is at least one project that need to send
	 * data.
	 */
	public class BootListener extends BroadcastReceiver
	{

		@Override
		public void onReceive(Context context, Intent intent)
		{
			Log.d(getClass().getSimpleName(), "Boot event received, starting " + DataSendingSchedulingService.class.getSimpleName() + " to schedule sending for all projects...");
			ScheduleAll(context);
		}

	}
	
}
