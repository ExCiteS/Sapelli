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
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.IntentService;
import android.content.Intent;

/**
 * Simple Service for scheduling alarms for project that need transmission.
 * 
 * @author Michalis Vitos, mstevens
 *
 */
public class AlarmScheduler extends IntentService implements StoreHandle.StoreUser
{
	
	private ProjectStore projectStore;
	private final CollectorApp app;

	/**
	 * A constructor is required, and must call the super IntentService(String) constructor with a name for the worker thread.
	 */
	public AlarmScheduler()
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
			for(Project p : projectStore.retrieveProjects())
			{
				// TODO if (p.isSending())
				// TODO interval should be saved in project -> p.getSendingInterval()
				SapelliAlarmManager.setAlarm(this, 60 * 1000, p.getID(), p.getFingerPrint());
				Debug.d("Projects: " + p.toString(true));
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
