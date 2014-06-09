package uk.ac.ucl.excites.sapelli.sender;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.SapelliCollectorClient;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.sender.gsm.SMSSender;
import uk.ac.ucl.excites.sapelli.sender.gsm.SignalMonitor;
import uk.ac.ucl.excites.sapelli.sender.util.SapelliAlarmManager;
import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
import uk.ac.ucl.excites.sapelli.util.Debug;
import uk.ac.ucl.excites.sapelli.util.DeviceControl;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;

/**
 * @author Michalis Vitos
 */
public class DataSenderService extends Service
{
	private BlockingQueue<Long> projectQueue = new ArrayBlockingQueue<Long>(1024);
	// Use a single Thread and Send the Projects sequential
	private ExecutorService projectsExecutor = Executors.newSingleThreadExecutor();

	@Override
	public synchronized int onStartCommand(Intent intent, int flags, int startId)
	{
		// TODO TEMP:
		long modelID = SapelliCollectorClient.GetSchemaID(intent.getExtras().getInt(SapelliAlarmManager.PROJECT_ID),
				intent.getExtras().getInt(SapelliAlarmManager.PROJECT_HASH));

		// Add project to queue:
		try
		{
			projectQueue.put(modelID);
		}
		catch(InterruptedException e)
		{
			e.printStackTrace();
		}

		// Run Projects
		runProjects();

		return Service.START_NOT_STICKY;
	}

	/**
	 * Check if the Thread is already running and execute the projectTask
	 */
	private void runProjects()
	{
		if(!projectsExecutor.isTerminated())
			projectsExecutor.execute(new Runnable()
			{
				@Override
				public void run()
				{
					while(!projectQueue.isEmpty())
						try
						{
							new ProjectSendingTask(DataSenderService.this, projectQueue.take());
						}
						catch(InterruptedException e)
						{
							Debug.e(e);
							break;
						}

					// Stop the Android Service
					stopSelf();
				}
			});
	}

	@Override
	public void onDestroy()
	{
		Debug.d("Service has been killed!");
	}

	@Override
	public IBinder onBind(Intent intent)
	{
		return null;
	}

	/**
	 * Transmitting the data for a project
	 * 
	 * @author Michalis Vitos
	 * 
	 */
	public class ProjectSendingTask implements StoreClient
	{
		private Context context;
		private Project project;
		private SMSTranmission sms;

		ProjectSendingTask(Context context, long modelID)
		{
			this.context = context;

			// Load the project
			try
			{
				ProjectStore store = ((CollectorApp) getApplication()).getProjectStore(this);
				this.project = store.retrieveProject(modelID); // TODO Call appropriate method
			}
			catch(Exception e)
			{
				Debug.e("Cannot Load Project: ", e);
			}

			// TODO Get Project Settings:
			if(project != null)
				project.isLogging();

			// TODO query for records

			// If project has records to send

			// if project needs SMS transmission
			if(sms == null)
				sms = new SMSTranmission(context);

			sms.send(project);

			// else if project needs Internet transmission

			// If there were sms transmissions, terminate them
			if(sms != null)
				sms.close();

			// TODO Remove
			final int time = 5;
			Debug.d("Project " + modelID + " is running and it takes " + time + " seconds");
			try
			{
				Thread.sleep(time * 1000);
			}
			catch(InterruptedException e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		public class SMSTranmission
		{
			private Context context;
			private SignalMonitor gsmMonitor;
			private SMSSender smsSender;

			public SMSTranmission(final Context context)
			{
				this.context = context;

				// 1. Check for Airplane Mode
				if(DeviceControl.canToogleAirplaneMode() && DeviceControl.inAirplaneMode(context) /* TODO && projects toggle AirplaneMode */)
					DeviceControl.toggleAirplaneMode(context, DeviceControl.POST_AIRPLANE_MODE_WAITING_TIME);

				// 2. Check for SMS Signal
				setupSMSmonitor(context);
			}

			public void send(Project p)
			{
				// 3. Send records
				if(gsmMonitor.isInService())
					// TODO Send them
					p.getDataFolderPath();
			}

			/**
			 * Stop the Signal Monitor and Toggle the AirplaneMode
			 */
			public void close()
			{
				// 1. Stop GSM Signal Monitor
				stopSingnalMonitor();

				// 2. Put device back to AirplaneMode if needed
				if(true /* TODO projects toggle AirplaneMode */)
					DeviceControl.toggleAirplaneMode(context);
			}

			/**
			 * Start a GSM SignalMonitor on the main UI thread by enqueuing the runnable action to be performed on a Handler
			 * 
			 * @param context
			 */
			private void setupSMSmonitor(final Context context)
			{
				Handler handler = new Handler(Looper.getMainLooper());
				handler.post(new Runnable()
				{
					@Override
					public void run()
					{
						gsmMonitor = new SignalMonitor(context);

					}
				});

				waitForSignalMonitor();
			}

			/**
			 * Wait for the Signal Monitor listener to be established
			 */
			private void waitForSignalMonitor()
			{
				while(gsmMonitor == null)
				{
					try
					{
						Thread.sleep(100);
					}
					catch(InterruptedException e)
					{
						Debug.e(e);
					}
				}

				Debug.d("Is in SMS service: " + gsmMonitor.isInService());
			}

			private void stopSingnalMonitor()
			{
				// Stop SignalMonitor Listener
				if(gsmMonitor != null)
				{
					gsmMonitor.stopSignalMonitor();
					gsmMonitor = null;
				}
			}
		}
	}

}