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
import uk.ac.ucl.excites.sapelli.collector.transmission.SendingSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.transmission.control.AndroidTransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import android.app.IntentService;
import android.content.Intent;
import android.util.Log;

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
	
	public static final String INTENT_KEY_PROJECT_ID = "projectId";
	public static final String INTENT_KEY_PROJECT_FINGERPRINT = "fingerPrint";
	
	private CollectorApp app;
	private AndroidTransmissionController transmissionController;
	
	public DataSendingService()
	{
		super(DataSendingService.class.getSimpleName());
	}

	@Override
	protected void onHandleIntent(Intent intent)
	{	
		Log.d(TAG, "Woken by alarm");
		// alarm has just woken up the service with a project ID and fingerprint
		
		// Read intent data:
		if(!intent.hasExtra(INTENT_KEY_PROJECT_ID) || !intent.hasExtra(INTENT_KEY_PROJECT_FINGERPRINT))
		{	// data missing from intent!
			Log.e(TAG,"Sender service woken by alarm but project data was missing from the Intent.");
			return;
		}
		int projectID = intent.getIntExtra(INTENT_KEY_PROJECT_ID, -1);
		int projectFingerPrint = intent.getIntExtra(INTENT_KEY_PROJECT_FINGERPRINT, -1);
		
		ProjectStore projectStore;
		TransmissionStore sentTStore;
		try
		{
			// do not get the app in the constructor(!):
			app = ((CollectorApp) getApplication());
			
			// Get transmission controller:
			transmissionController = new AndroidTransmissionController(app);
			
			// Get ProjectStore instance:
			projectStore = app.collectorClient.projectStoreHandle.getStore(this);
			
			// Get SentTransmissionStore instance:
			sentTStore = app.collectorClient.transmissionStoreHandle.getStore(this);
			
			// Get Project:
			Project project = projectStore.retrieveProject(projectID, projectFingerPrint);
			if(project == null)
			{
				transmissionController.addLogLine(TAG, "Data sending canceled, project (ID: " + projectID + "; fingerprint: " + projectFingerPrint + ") not found.");
				DataSendingSchedulingService.Cancel(app, projectID, projectFingerPrint); // cancel future alarms for this project
				return;
			}
			
			// Get Schedule/Receiver:
			SendingSchedule sendSchedule = projectStore.retrieveSendScheduleForProject(project, sentTStore);
			if(sendSchedule == null)
			{
				transmissionController.addLogLine(TAG, "Data sending canceled, no schedule/receiver found for project " + project.toString(true));
				DataSendingSchedulingService.Cancel(app, project); // cancel future alarms for this project
				return;
			}
			
			// TODO detect network reception/connectivity...
	
			// Send records:
			transmissionController.sendRecords(project.getModel(), sendSchedule.getReceiver());
			
			// TOOD send files?
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon trying to send data for project with ID " + projectID + " and finger print " + projectFingerPrint, e);
		}
		finally
		{
			app.collectorClient.projectStoreHandle.doneUsing(this);
			app.collectorClient.transmissionStoreHandle.doneUsing(this);
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