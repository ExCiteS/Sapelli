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
import uk.ac.ucl.excites.sapelli.collector.remote.SendRecordsSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import android.app.IntentService;
import android.content.Intent;
import android.util.Log;

/**
 * Simple Service for scheduling alarms for projects that need transmission (activated on device boot).
 * 
 * @author Michalis Vitos, mstevens, benelliott
 *
 */
public class SendAlarmInitialiserService extends IntentService implements StoreHandle.StoreUser
{
	private static final String TAG = SendAlarmInitialiserService.class.getSimpleName();
	
	private CollectorApp app;

	private ProjectStore projectStore;
	private TransmissionStore sentTxStore;

	/**
	 * A constructor is required, and must call the super IntentService(String) constructor with a name for the worker thread.
	 */
	public SendAlarmInitialiserService()
	{
		super("AlarmScheduler");
	}

	/**
	 * The IntentService calls this method from the default worker thread with the intent that started the service. When this method returns, IntentService
	 * stops the service, as appropriate.
	 */
	@Override
	protected void onHandleIntent(Intent intent)
	{
		// Check if projects require data transmission and set up alarms for the DataSenderService
		Log.d(TAG, "Scanning projects for alarms that need to be set");
		try
		{
			// do not call this in the constructor!!:
			app = ((CollectorApp) getApplication());

			// Get ProjectStore instance:
			if(projectStore == null || projectStore.isClosed())
				projectStore = app.collectorClient.projectStoreHandle.getStore(this);
			
			// Get SentTransmissionStore instance:
			if(sentTxStore == null || sentTxStore.isClosed())
				sentTxStore = app.collectorClient.sentTransmissionStoreHandle.getStore(this);
			
			// Set an Alarm, for each of the projects that has sending enabled
			for(Project project : projectStore.retrieveProjects())// TODO projectStore.getSendingProjects?
			{
				SendRecordsSchedule sendSchedule = projectStore.retrieveSendScheduleForProject(project, sentTxStore);
				if(sendSchedule != null)
				{
					SendAlarmManager.setSendRecordsAlarm(this, sendSchedule.getRetransmitIntervalMillis(), project.getID(), project.getFingerPrint());
					Log.d(TAG, "Set send alarm for project "+project.getID()+", interval: "+sendSchedule.getRetransmitIntervalMillis()+"ms, receiver name: "+sendSchedule.getReceiver().getName());
				}
				else 
				{
					Log.d(TAG, "No alarm found for project "+project.getID());
				}
			}
			Log.d(TAG, "No project left to check");
		}
		catch(Exception e)
		{
			Log.d(TAG, "Exception while looking for projects that need alarms", e);
		}
	}
}
