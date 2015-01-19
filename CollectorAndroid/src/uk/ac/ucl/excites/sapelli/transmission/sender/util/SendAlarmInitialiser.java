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
import android.app.IntentService;
import android.content.Intent;
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
				if (receiver != null)
				{
					SendAlarmManager.setAlarm(this, receiver.getRetransmitIntervalMillis(), project.getID(), project.getFingerPrint());
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
	
}
