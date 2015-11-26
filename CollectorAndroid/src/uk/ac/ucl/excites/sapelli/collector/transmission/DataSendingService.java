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

import android.app.IntentService;
import android.content.Intent;
import android.util.Log;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.transmission.control.AndroidTransmissionController;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.shared.util.android.SignalMonitor;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * IntentService which is awoken by a SendAlarm intent to send any pending Records for a particular Project according to the SendRecordsSchedule that it retrieves from the ProjectStore.
 * <br>
 * <br>
 * Note that Android queues requests to the same service and they are dealt with one-by-one in a worker thread (off the main thread). Using an IntentService means that we can let Android handle the queueing of several
 * requests to the same Intent for us. However the Android documentation for an IntentService is somewhat ambiguous about exactly when the IntentService object itself is destroyed
 *  ("IntentService will receive the Intents, launch a worker thread, and stop the service as appropriate", see <a href=http://developer.android.com/reference/android/app/IntentService.html>the official documentation</a>).
 * <br>
 * <br>
 * Hence all the checks to see if we already have resources such as Stores and Controllers or whether we need to (re-)initialise them. This could be an argument for explicitly managing the queue ourselves, but remember that
 * the chances of a high number of send-record alarms going off at roughly the same time are quite low (would require very short intervals or synchronised intervals and a lot of actively sending projects).
 * 
 * @author Michalis Vitos, benelliott, mstevens
 */
public class DataSendingService extends IntentService implements StoreUser
{
	
	private static final String TAG = DataSendingService.class.getSimpleName();
	
	/*package*/ static String getIntentAction(int projectID, int projectFingerPrint)
	{
		return "SendDataForProject:" + projectID + "_" + projectFingerPrint;
	}
	
	public static final String INTENT_KEY_SEND_SCHEDULE_ID = "sendScheduleId";
	
	private CollectorApp app;
	private AndroidTransmissionController transmissionController;
	private SignalMonitor signalMonitor;
	
	public DataSendingService()
	{
		super(DataSendingService.class.getSimpleName());
	}

	/* (non-Javadoc)
	 * @see android.app.IntentService#onStart(android.content.Intent, int)
	 */
	@Override
	public void onStart(Intent intent, int startId)
	{
		super.onStart(intent, startId);
		
		signalMonitor = SignalMonitor.Start(getApplicationContext());
	}

	@Override
	protected void onHandleIntent(Intent intent)
	{	
		Log.d(TAG, "Woken by alarm");
		// Alarm has just woken up the service with a project ID and fingerprint
		
		// Read intent data:
		if(!intent.hasExtra(INTENT_KEY_SEND_SCHEDULE_ID))
		{	// data missing from intent!
			Log.e(TAG,"Sender service woken by alarm but " + INTENT_KEY_SEND_SCHEDULE_ID + " was missing from the Intent.");
			return;
		}
		
		SendSchedule sendSchedule = null;
		ProjectStore projectStore;
		TransmissionStore transmissionStore;
		try
		{
			// do not get the app in the constructor(!):
			app = ((CollectorApp) getApplication());
			
			// Get transmission controller:
			transmissionController = new AndroidTransmissionController(app);
			
			// Get ProjectStore instance:
			projectStore = app.collectorClient.projectStoreHandle.getStore(this);
			
			// Get SentTransmissionStore instance:
			transmissionStore = app.collectorClient.transmissionStoreHandle.getStore(this);
			
			// Get Schedule:
			sendSchedule = projectStore.retrieveSendScheduleByID(intent.getIntExtra(INTENT_KEY_SEND_SCHEDULE_ID, -1), transmissionStore);
			if(sendSchedule == null || !sendSchedule.isEnabled())
				return;
			
			// Attempt transmission:
			transmit(sendSchedule, false);
		}
		catch(Exception e)
		{
			if(sendSchedule != null)
				Log.e(TAG, "Error upon trying to send data for project with ID " + sendSchedule.getProject().getID() + " and finger print " + sendSchedule.getProject().getFingerPrint(), e);
			else
				Log.e(TAG, "onHandleIntent()", e);
		}
		finally
		{
			app.collectorClient.projectStoreHandle.doneUsing(this);
			app.collectorClient.transmissionStoreHandle.doneUsing(this);
		}
	}
	
	public void transmit(SendSchedule sendSchedule, boolean leftAirplaneMode) throws Exception
	{
		if(isOnline(sendSchedule))
		{	// We are online...
			//	Send records:
			transmissionController.sendRecords(sendSchedule.getProject().getModel(), sendSchedule.getReceiver());
			//	TODO send files?
		}
		else if(!leftAirplaneMode && DeviceControl.inAirplaneMode(getApplicationContext()) && DeviceControl.canSetAirplaneMode() && sendSchedule.isAirplaneModeCycling())
		{	// We are in airplane mode and we can and are configured to control it:
			//	Leave airplane mode & wait...
			DeviceControl.disableAirplaneModeAndWait(getApplicationContext(), DeviceControl.POST_AIRPLANE_MODE_WAITING_TIME_S);
			//	Now try again:
			transmit(sendSchedule, true);
		}
		else
		{	// We are offline...
			transmissionController.addLogLine(TAG, "Skipping transmission attempt for project (" + sendSchedule.getProject().toString(false) + ") due to lack of connectivity");
		}
	}
	
	public boolean isOnline(SendSchedule sendSchedule)
	{
		switch(sendSchedule.getReceiver().getTransmissionType())
		{
			case TEXTUAL_SMS:
			case BINARY_SMS:
				return signalMonitor.isInService();
			case HTTP:
				return DeviceControl.isOnline(getApplicationContext());
			default:
				Log.e(TAG, "Unsupported transmission type: " + sendSchedule.getReceiver().getTransmissionType().name());
				return false;
		}
	}
	
	/* (non-Javadoc)
	 * @see android.app.IntentService#onDestroy()
	 */
	@Override
	public void onDestroy()
	{
		if(transmissionController != null)
			transmissionController.discard();
		if(signalMonitor != null)
			signalMonitor.stop();
		
		super.onDestroy();
	}
	
}
