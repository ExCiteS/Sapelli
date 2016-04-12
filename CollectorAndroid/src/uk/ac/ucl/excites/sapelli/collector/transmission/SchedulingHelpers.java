/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

import android.app.AlarmManager;
import android.app.IntentService;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.SystemClock;
import android.util.Log;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;

/**
 * Simple Service for scheduling alarms for projects that need transmission (activated on device boot).
 * 
 * @author Michalis Vitos, mstevens, benelliott
 */
public final class SchedulingHelpers
{
	
	static public final int ANDROID_22_MINIMAL_ALARM_INTERVAL_SECONDS = 60;
	
	/**
	 * Checks if the interval is no too short.
	 * Also accounts for the (undocumented!) change in Android_v5.1/API22 which enforces a minimum of 60 seconds.
	 * 
	 * @param transmitIntervalS the schedule interval as configured by the user (in seconds)
	 * @return the effective interval as allowed on the device (also in seconds)
	 * 
	 * @see https://code.google.com/p/android/issues/detail?id=161244
	 */
	static public int getEffectiveAlarmIntervalSeconds(int transmitIntervalS)
	{
		if(transmitIntervalS < SendSchedule.MINIMUM_TRANSMIT_INTERVAL_SECONDS)
			transmitIntervalS = SendSchedule.MINIMUM_TRANSMIT_INTERVAL_SECONDS;
		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP_MR1 && transmitIntervalS < ANDROID_22_MINIMAL_ALARM_INTERVAL_SECONDS)
			transmitIntervalS = ANDROID_22_MINIMAL_ALARM_INTERVAL_SECONDS;
		return transmitIntervalS;
	}
	
	private SchedulingHelpers() {}

	private static final String TAG = SchedulingHelpers.class.getSimpleName();

	/**
	 * Default delay (of half a minute) between setting of alarm for a SendSchedule and the time it first goes off.
	 */
	private static final int DEFAULT_ALARM_DELAY_MS = 30 * 1000;
	
	/**
	 * @param context
	 * @param sendSchedule
	 */
	public static void ScheduleForImmediateTransmission(Context context, SendSchedule sendSchedule)
	{
		context.startService(GetDataSendingServiceIntent(context, sendSchedule.getID()));
	}
	
	/**
	 * Sets or cancels an alarm for the given SendSchedule depending on whether it is enabled or not.
	 * 
	 * @param context
	 * @param sendSchedule
	 */
	public static void ScheduleOrCancel(Context context, SendSchedule sendSchedule)
	{
		if(sendSchedule == null)
			return;
		if(sendSchedule.isEnabled())
			// Schedule:
			Schedule(context, sendSchedule); // also sets bootlistener
		else
			// Cancel:
			Cancel(context, sendSchedule);
	}
	
	/**
	 * Set up an Alarm for the given SendSchedule, initially after {@link DEFAULT_ALARM_DELAY_MS} and then every <code>sendSchedule.getTransmitIntervalS()</code>
	 * 
	 * @param sendSchedule
	 */
	public static void Schedule(Context context, SendSchedule sendSchedule)
	{
		Schedule(context, sendSchedule, true);
	}
	
	/**
	 * Set up an Alarm for the given SendSchedule, initially after {@link DEFAULT_ALARM_DELAY_MS} and then every <code>sendSchedule.getTransmitIntervalS()</code>.
	 * 
	 * @param sendSchedule
	 * @param setBootListener
	 * @return whether or not the alarm was set
	 */
	private static boolean Schedule(Context context, SendSchedule sendSchedule, boolean setBootListener)
	{
		return Schedule(context, DEFAULT_ALARM_DELAY_MS, sendSchedule, setBootListener);
	}
	
 	/**
	 * Set up an Alarm for the given SendSchedule, initially after <code>triggerAtMillis</code> and then every <code>sendSchedule.getTransmitIntervalS()</code>.
	 * 
	 * @param context
	 * @param triggerDelay
	 * @param sendSchedule
	 * @param setBootListener
	 * @return whether or not the alarm was set
	 */
	private static boolean Schedule(Context context, int triggerDelay, SendSchedule sendSchedule, boolean setBootListener)
	{
		if(!SendSchedule.isValidForTransmission(sendSchedule))
		{
			// this schedule is not valid, cancel existing alarm:
			if(sendSchedule.isIDSet())
				Cancel(context, sendSchedule.getID());
			// we did not set the alarm:
			return false;
		}
		
		// Set the alarm to be triggered every intervalMillis:
		try
		{
			int intervalMillis = sendSchedule.getTransmitIntervalS() * 1000;
			GetAlarmManager(context).setRepeating(
				AlarmManager.ELAPSED_REALTIME,
				SystemClock.elapsedRealtime() + triggerDelay,
				intervalMillis,
				GetDataSendingPendingIntent(context, sendSchedule.getID()));
				
			Log.d(TAG, "Set SendSchedule (id: " + sendSchedule.getID() + ") alarm for project \"" + sendSchedule.getProject().toString(false) + "\", and receiver \"" + sendSchedule.getReceiver().toString() + "\" to expire every " + intervalMillis + "ms after a delay of " + triggerDelay + "ms.");
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon scheduling alarm", e);
			return false;
		}
		
		// setup the bootlistener if needed:
		if(setBootListener)
			setupBootReceiver(context, true);
		
		// report we set the alarm:
		return true;
	}
	
	/**
	 * Cancel alarm for the given SendSchedule.
	 * 
	 * @param context
	 * @param sendSchedule
	 */
	public static void Cancel(Context context, SendSchedule sendSchedule)
	{
		if(sendSchedule != null && sendSchedule.isIDSet())
			Cancel(context, sendSchedule.getID());
	}
	
	/**
	 * Cancel alarm for the given SendSchedule.
	 * 
	 * @param context
	 * @param alarmManager
	 * @param project
	 */
	public static void Cancel(Context context, int sendScheduleId)
	{
		try
		{
			GetAlarmManager(context).cancel(GetDataSendingPendingIntent(context, sendScheduleId));
			Log.d(TAG, "Canceled alarm for SendSchedule with id " + sendScheduleId);
		}
		catch(Exception e)
		{
			Log.e(TAG, "Exception upon cancelling alarm", e);
		}
	}
	
	/**
	 * @param context
	 * @return the AlarmManager
	 */
	private static AlarmManager GetAlarmManager(Context context)
	{
		return (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
	}
	
	private static Intent GetDataSendingServiceIntent(Context context, int sendScheduleID)
	{
		// Create the PendingIntent for the DataSenderService:
		Intent serviceIntent = new Intent(context, DataSendingService.class);
		// Action (matched by Intent.filterEquals(Intent)):
		serviceIntent.setAction(DataSendingService.getIntentAction(sendScheduleID));
		return serviceIntent;
	}
	
	/**
	 * @return PendingIntent to start DataSendingService to send data according to the SendSchedule with given id.
	 */
	private static PendingIntent GetDataSendingPendingIntent(Context context, int sendScheduleID)
	{
		return PendingIntent.getService(context, sendScheduleID, GetDataSendingServiceIntent(context, sendScheduleID), PendingIntent.FLAG_UPDATE_CURRENT);
	}
	
	/**
	 * @param enabled
	 */
	static private void setupBootReceiver(Context context, boolean enabled)
	{
		ComponentName receiver = new ComponentName(context, BootListener.class);
		int newState = (enabled) ? PackageManager.COMPONENT_ENABLED_STATE_ENABLED : PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
		context.getPackageManager().setComponentEnabledSetting(receiver, newState, PackageManager.DONT_KILL_APP);
		Log.d(TAG, (enabled ? "En" : "Dis") + "abled " + BootListener.class.getSimpleName());
	}
	
	/**
	 * Start the service with as task of setting alarms for the SendSchedules of all projects.
	 * 
	 * @param context
	 */
	public static void ScheduleAll(Context context)
	{
		context.startService(new Intent(context, SchedulingHelpers.SchedulingService.class));
	}
	
	/**
	 * @author mstevens
	 *
	 */
	static public class SchedulingService extends IntentService implements StoreHandle.StoreUser
	{

		/**
		 * A constructor is required, and must call the super IntentService(String) constructor with a name for the worker thread.
		 */
		public SchedulingService()
		{
			super(SchedulingHelpers.SchedulingService.class.getSimpleName());
		}

		/**
		 * The IntentService calls this method from the default worker thread with the intent that started the service. When this method returns, IntentService
		 * stops the service, as appropriate.
		 */
		@Override
		protected void onHandleIntent(Intent intent)
		{
			Log.d(SchedulingHelpers.class.getSimpleName(), "Starting alarm scheduler...");

			CollectorApp app = null;
			ProjectStore projectStore;
			try
			{
				// Get app & alarm manager (do not call this in the constructor!):
				app = ((CollectorApp) getApplication());

				// Get ProjectStore instance:
				projectStore = app.collectorClient.projectStoreHandle.getStore(this);

				// Set an alarm for each (valid/enabled) SendSchedule of each Project:
				boolean enableBootReceiver = false;
				for(SendSchedule sendSchedule : projectStore.retrieveEnabledSendSchedules())
					enableBootReceiver |= Schedule(getApplicationContext(), sendSchedule, false);

				// If we have at least one project needs to send, so make sure alarms are re-enabled on boot (if not boot receiver is disabled):
				setupBootReceiver(app, enableBootReceiver);
			}
			catch(Exception e)
			{
				Log.d(TAG, "Exception while setting alarms for all SendSchedules of all Projects.", e);
			}
			finally
			{
				if(app != null)
					app.collectorClient.projectStoreHandle.doneUsing(this);
			}
		}

	}

	/**
	 * BroadcastReceiver that listens for device boot events and when one is received, starts the SendAlarmInitialiser service.
	 * 
	 * Note that this BroadcastReceiver is only registered to listen for boot events if Sapelli determines that there is at least one project that need to send data.
	 */
	static public class BootListener extends BroadcastReceiver
	{

		@Override
		public void onReceive(Context context, Intent intent)
		{
			Log.d(getClass().getSimpleName(), "Boot event received, starting " + SchedulingService.class.getSimpleName() + " to schedule sending for all projects...");
			ScheduleAll(context);
		}

	}
	
}
