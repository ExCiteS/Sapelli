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

package uk.ac.ucl.excites.sapelli.collector.services;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.remote.SendRecordsSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
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

	/**
	 * Start the service with as task the scheduling of all projects that need sending
	 * 
	 * @param context
	 */
	public static void ScheduleAll(Context context)
	{
		Intent scheduleIntent = new Intent(context, DataSendingSchedulingService.class);
		context.startService(scheduleIntent);
	}
	
	/**
	 * Cancel scheduled sending of data associated with the given project.
	 * 
	 * @param context
	 * @param project
	 */
	public static void Cancel(Context context, Project project)
	{
		Cancel(context, project.getID(), project.getFingerPrint());
	}
	
	/**
	 * Cancel scheduled sending of data associated with the project with the given ID & finger print.
	 * 
	 * @param context
	 * @param projectID
	 * @param projectFingerPrint
	 */
	public static void Cancel(Context context, int projectID, int projectFingerPrint)
	{
		try
		{
			((AlarmManager) context.getSystemService(Context.ALARM_SERVICE)).cancel(GetDataSendingIntent(context, projectID, projectFingerPrint));
		}
		catch(Exception ignore) {}
	}
	
	/**
	 * @param projectID
	 * @param projectFingerPrint
	 * @return PendingIntent to start DataSendingService to send data for the given project
	 */
	private static PendingIntent GetDataSendingIntent(Context context, int projectID, int projectFingerPrint)
	{
		// Create the PendingIntent for the DataSenderService
		Intent serviceIntent = new Intent(context, DataSendingService.class);
		serviceIntent.putExtra(DataSendingService.INTENT_KEY_PROJECT_ID, projectID);
		serviceIntent.putExtra(DataSendingService.INTENT_KEY_PROJECT_FINGERPRINT, projectFingerPrint);
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

		ProjectStore projectStore;
		TransmissionStore sentTStore;
		try
		{
			// Get ProjectStore instance:
			projectStore = app.collectorClient.projectStoreHandle.getStore(this);
			// Get SentTransmissionStore instance:
			sentTStore = app.collectorClient.sentTransmissionStoreHandle.getStore(this);

			// Check if projects require data transmission and set up alarms for the DataSenderService
			Log.d(TAG, "Scanning projects for alarms that need to be set");
	
			// Set an Alarm, for each of the projects that has sending enabled
			boolean atLeastOne = false;
			for(Project project : projectStore.retrieveProjects())
			{
				SendRecordsSchedule sendSchedule = projectStore.retrieveSendScheduleForProject(project, sentTStore);
				if(sendSchedule != null)
				{
					scheduleSending(sendSchedule.getRetransmitIntervalMillis(), project);
					atLeastOne = true;
				}
				else
				{
					Log.d(TAG, "No schedule found for project \"" + project.toString(false) + "\"");
					// Cancel any previously set alarm:
					cancelSending(project);
				}
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
			app.collectorClient.sentTransmissionStoreHandle.doneUsing(this);
		}
	}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSendingService}, initially after a minute and then every <code>intervalMillis</code>
	 * 
	 * @param intervalMillis
	 * @param project
	 */
	private void scheduleSending(int intervalMillis, Project project)
	{
		scheduleSending(DEFAULT_DELAY_MILLIS, intervalMillis, project);
	}

	/**
	 * Set up an Alarm for a project that calls the {@link DataSendingService}, initially after <code>triggerAtMillis</code> and then every
	 * <code>intervalMillis</code>
	 * 
	 * @param triggerDelay
	 * @param intervalMillis
	 * @param project
	 */
	private void scheduleSending(int triggerDelay, int intervalMillis, Project project)
	{
		// Cancel existing alarm if there is one:
		cancelSending(project);

		// Setup the alarm to be triggered every intervalMillis
		alarmManager.setRepeating(AlarmManager.ELAPSED_REALTIME_WAKEUP, SystemClock.elapsedRealtime() + triggerDelay, intervalMillis, getDataSendingIntent(project)); // TODO should we really be waking up the device for this?
		Log.d(TAG, "Set sending alarm for project \"" + project.toString(false) + "\" to expire every " + intervalMillis + "ms after a delay of " + triggerDelay + "ms.");
	}

	/**
	 * @param project
	 */
	private void cancelSending(Project project)
	{
		alarmManager.cancel(getDataSendingIntent(project));
	}

	/**
	 * @param project
	 * @return PendingIntent to start DataSendingService to send data for the given project
	 */
	private PendingIntent getDataSendingIntent(Project project)
	{
		return GetDataSendingIntent(app, project.getID(), project.getFingerPrint());
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
