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

package uk.ac.ucl.excites.sapelli.transmission.sender.util;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.remote.Receiver;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.transmission.sender.RecordSenderService;
import uk.ac.ucl.excites.sapelli.transmission.sender.SendAlarmBootListener;
import android.app.AlarmManager;
import android.app.IntentService;
import android.app.PendingIntent;
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
public class SendAlarmInitialiser extends IntentService implements StoreHandle.StoreUser
{
	
	private ProjectStore projectStore;
	private final CollectorApp app;
	/**
	 * A constructor is required, and must call the super IntentService(String) constructor with a name for the worker thread.
	 */
	public SendAlarmInitialiser()
	{
		super("AlarmScheduler");
		app = ((CollectorApp) getApplication());
	}

	/**
	 * The IntentService calls this method from the default worker thread with the intent that started the service. When this method returns, IntentService
	 * stops the service, as appropriate.
	 */
	@Override
	protected void onHandleIntent(Intent intent)
	{
		// Check if projects require data transmission and set up alarms for the DataSenderService
		try
		{
			// Get ProjectStore instance:
			if(projectStore == null || projectStore.isClosed())
				projectStore = app.collectorClient.projectStoreHandle.getStore(this);
			
			// Set an Alarm, for each of the projects that has sending enabled
			for(Project project : projectStore.retrieveProjects())// TODO projectStore.getSendingProjects?
			{
				Receiver receiver = projectStore.retrieveReceiverForProject(project);
				if(receiver != null)
				{
					SendAlarmManager.setSendRecordsAlarm(this, receiver.getRetransmitIntervalMillis(), project.getID(), project.getFingerPrint());
					Log.d(SendAlarmInitialiser.class.getName(), "Set send alarm for project "+project.getID()+", interval: "+receiver.getRetransmitIntervalMillis()+"ms, receiver name: "+receiver.getCorrespondent().getName());
				}
			}
		}
		catch(Exception e)
		{
			// TODO
		}
	}

	@Override
	public void onDestroy()
	{
		super.onDestroy();
		if(projectStore != null)
			app.collectorClient.projectStoreHandle.doneUsing(this);
	}
	
	public static final String INTENT_KEY_PROJECT_ID = "projectId";
	public static final String INTENT_KEY_PROJECT_FINGERPRINT = "fingerPrint";

	/**
	 * Set up an Alarm for a project that calls the {@link DataSenderService}, initially after a minute and then every <code>intervalMillis</code>
	 * 
	 * @param context
	 * @param intervalMillis
	 * @param projectID
	 * @param fingerPrint
	 */
	public static void setSendRecordsAlarm(Context context, int intervalMillis, int projectID, int fingerPrint)
	{
		setSendRecordsAlarm(context, 60 * 1000, intervalMillis, projectID, fingerPrint);
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
	public static void setSendRecordsAlarm(Context context, int triggerDelay, int intervalMillis, int projectID, int fingerPrint)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);

		// Setup the alarm to be triggered every intervalMillis
		alarmManager.setRepeating(AlarmManager.ELAPSED_REALTIME_WAKEUP, SystemClock.elapsedRealtime() + triggerDelay, intervalMillis,
				getRecordsAlarmIntent(context, projectID, fingerPrint));

		// We know at least one project needs to send, so make sure alarms are re-enabled on boot:
		enableBootReceiver(context, true);
	}


	/**
	 * @param context
	 * @param projectID
	 * @param fingerPrint
	 */
	public static void cancelSendRecordsAlarm(ProjectStore projectStore, Context context, int projectID, int fingerPrint)
	{
		// Create Alarm Manager
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
		alarmManager.cancel(getRecordsAlarmIntent(context, projectID, fingerPrint));

		// Check whether or not we need to worry about re-enabling alarms on boot:
		checkBootReceiver(projectStore, context);
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
	private static void checkBootReceiver(ProjectStore projectStore, Context context)
	{
		boolean isSending = false;

		// check if any of the projects in the project store need the boot receiver to be enabled:
		for(Project p : projectStore.retrieveProjects()) // TODO projectStore.getSendingProjects?
		{
			Receiver receiver = projectStore.retrieveReceiverForProject(p);
			if (receiver != null)
			{
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
	}
}
