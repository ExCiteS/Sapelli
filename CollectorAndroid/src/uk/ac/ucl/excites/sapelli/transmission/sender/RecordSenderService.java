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

package uk.ac.ucl.excites.sapelli.transmission.sender;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.remote.Receiver;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.transmission.control.AndroidTransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.sender.util.SendAlarmManager;
import android.app.IntentService;
import android.content.Intent;
import android.util.Log;

/**
 * 
 * @author Michalis Vitos, benelliott
 */
public class RecordSenderService extends IntentService implements StoreUser
{
	private static final String TAG = RecordSenderService.class.getName();
	
	public RecordSenderService()
	{
		super("Sapelli Record Sender");
	}

	@Override
	protected void onHandleIntent(Intent intent)
	{	
		// alarm has just woken up the service with a project ID and fingerprint
		int projectID = intent.getIntExtra(SendAlarmManager.INTENT_KEY_PROJECT_ID, -1);
		
		int projectFingerprint = intent.getIntExtra(SendAlarmManager.INTENT_KEY_PROJECT_FINGERPRINT, -1);
		
		if (projectID == -1 || projectFingerprint == -1)
		{
			// data missing from intent!
			Log.e(TAG,"Sender service woken by alarm but project data was missing from the Intent.");
			return;
		}
		
		try
		{
			ProjectStore projectStore = ((CollectorApp) getApplication()).collectorClient.projectStoreHandle.getStore(this);
			
			Project project = projectStore.retrieveProject(projectID, projectFingerprint);
			
			if (project == null)
				throw new Exception("Project with ID "+projectID+" and fingerprint "+projectFingerprint+" was not found in project store");
			
			Receiver receiver = projectStore.retrieveReceiverForProject(project);
			
			if (receiver == null)
				throw new Exception("Could not find receiver for project with ID "+projectID+" and fingerprint "+projectFingerprint+".");
			
			AndroidTransmissionController transmissionController = new AndroidTransmissionController(((CollectorApp) getApplication()).collectorClient, ((CollectorApp) getApplication()).getFileStorageProvider(), this);
			
			transmissionController.sendRecords(project.getModel(), receiver.getCorrespondent());
			
		}
		catch(Exception e)
		{
			Log.e(TAG, "Sender service woken by alarm but some necessary data was not successfully retreived from the database", e);
		}
	}

	

}
//
//
//public class DataSenderService extends Service
//{
//	// Create a Queue of local TempProjects TODO: get rid of local TempProject class
//	private BlockingQueue<TempProject> projectQueue = new ArrayBlockingQueue<TempProject>(1024);
//	// Use a single Thread and Send the Projects sequential
//	private ExecutorService projectsExecutor = Executors.newSingleThreadExecutor();
//
//	private CollectorApp app;
//	
//	@Override
//	public synchronized int onStartCommand(Intent intent, int flags, int startId)
//	{
//		app = (CollectorApp) getApplication();
//		
//		// TODO TEMP:
//		TempProject p = new TempProject(intent.getExtras().getInt(SendAlarmManager.PROJECT_ID), intent.getExtras().getInt(SendAlarmManager.PROJECT_FINGERPRINT));
//
//		// Add project to queue:
//		try
//		{
//			projectQueue.put(p);
//		}
//		catch(InterruptedException e)
//		{
//			Debug.e(e);
//		}
//
//		// Run Projects in the queue:
//		if(!projectsExecutor.isTerminated())
//			projectsExecutor.execute(new Runnable()
//			{
//				@Override
//				public void run()
//				{
//					while(!projectQueue.isEmpty())
//						try
//						{
//							new ProjectSendingTask(DataSenderService.this, projectQueue.take());
//						}
//						catch(InterruptedException e)
//						{
//							Debug.e(e);
//							break;
//						}
//
//					// Stop the Android Service
//					stopSelf();
//				}
//			});
//
//		return Service.START_NOT_STICKY;
//	}
//
//	@Override
//	public void onDestroy()
//	{
//		Debug.d("Service has been killed!");
//	}
//
//	@Override
//	public IBinder onBind(Intent intent)
//	{
//		return null;
//	}
//
//	private class TempProject
//	{
//		int id;
//		int fingerprint;
//
//		TempProject(int id, int fingerprint)
//		{
//			this.id = id;
//			this.fingerprint = fingerprint;
//		}
//
//		/**
//		 * @return the id
//		 */
//		public int getId()
//		{
//			return id;
//		}
//
//		/**
//		 * @return the fingerprint
//		 */
//		public int getFingerprint()
//		{
//			return fingerprint;
//		}
//	}
//
//	/**
//	 * Transmitting the data for a project
//	 * 
//	 * @author Michalis Vitos
//	 * 
//	 */
//	public class ProjectSendingTask implements StoreHandle.StoreUser
//	{
//		private Project project;
//		private SMSSendingTask smsSendingTask;
//
//		ProjectSendingTask(Context context, TempProject p)
//		{
//			// Load the project
//			try
//			{
//				ProjectStore store = app.collectorClient.projectStoreHandle.getStore(this);
//				this.project = store.retrieveProject(p.getId(), p.getFingerprint());
//			}
//			catch(Exception e)
//			{
//				Debug.e("Cannot Load Project: ", e);
//			}
//
//			// TODO Get Project Settings:
//			if(project != null)
//			{
//				project.isLogging();
//
//				// TODO query for records
//
//				// TODO If project has records to send
//
//				// TODO if project needs SMS transmission
//				if(smsSendingTask == null)
//					smsSendingTask = new SMSSendingTask(context);
//
//				smsSendingTask.send(project);
//
//				// TODO else if project needs Internet transmission
//
//				// If there were SMS Sending Tasks, terminate them
//				if(smsSendingTask != null)
//				{
//					smsSendingTask.close();
//					smsSendingTask = null;
//				}
//			}
//		}
//
//		public class SMSSendingTask
//		{
//			private Context context;
//			private SignalMonitor gsmMonitor;
//
//			public SMSSendingTask(final Context context)
//			{
//				this.context = context;
//
//				// Check for Airplane Mode
//				if(DeviceControl.canToogleAirplaneMode() /* TODO && project needs to change AirplaneMode */)
//					DeviceControl.disableAirplaneModeAndWait(context, DeviceControl.POST_AIRPLANE_MODE_WAITING_TIME);
//
//				// Check for SMS Signal
//				setupSMSmonitor(context);
//			}
//
//			public void send(Project p)
//			{
//				// TODO get List of Records
//
//				// Send records
//				if(gsmMonitor.isInService())
//				{
//					// TODO Send them
//
//					// Test
//					AndroidSMSSender smsSender = new AndroidSMSSender(context);
//					// smsSender.send("Sapelli SMS Demo!!! " + TimeUtils.getPrettyTimestamp(), "phone");
//				}
//			}
//
//			/**
//			 * Stop the Signal Monitor and Toggle the AirplaneMode
//			 */
//			public void close()
//			{
//				// Stop GSM Signal Monitor
//				stopSingnalMonitor();
//
//				// Put device back to AirplaneMode if needed
//				if(true /* TODO projects toggle AirplaneMode */)
//					DeviceControl.enableAirplaneMode(context);
//			}
//
//			/**
//			 * Start a GSM SignalMonitor on the main UI thread by enqueuing the runnable action to be performed on a Handler
//			 * 
//			 * @param context
//			 */
//			private void setupSMSmonitor(final Context context)
//			{
//				Handler handler = new Handler(Looper.getMainLooper());
//				handler.post(new Runnable()
//				{
//					@Override
//					public void run()
//					{
//						gsmMonitor = new SignalMonitor(context);
//
//					}
//				});
//
//				// Wait for the Signal Monitor listener to be established
//				while(gsmMonitor == null)
//				{
//					try
//					{
//						Thread.sleep(100);
//					}
//					catch(InterruptedException e)
//					{
//						Debug.e(e);
//					}
//				}
//			}
//
//			private void stopSingnalMonitor()
//			{
//				// Stop SignalMonitor Listener
//				if(gsmMonitor != null)
//				{
//					gsmMonitor.stopSignalMonitor();
//					gsmMonitor = null;
//				}
//			}
//		}
//	}
//
//}