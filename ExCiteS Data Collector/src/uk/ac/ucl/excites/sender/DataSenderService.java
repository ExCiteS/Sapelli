package uk.ac.ucl.excites.sender;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.sender.dropbox.DropboxSync;
import uk.ac.ucl.excites.sender.gsm.SMSSender;
import uk.ac.ucl.excites.sender.gsm.SignalMonitor;
import uk.ac.ucl.excites.sender.util.Constants;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.util.DeviceControl;
import uk.ac.ucl.excites.util.Logger;
import android.app.Notification;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;


/**
 * A service that checks about network connectivity and tries to send the data Also the service tries to upload file attachments to Dropbox
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
public class DataSenderService extends Service
{

	// Statics-------------------------------------------------------
	static private final String TAG = "DataSenderService";
	static private final String LOG_PREFIX = "DataSender_";
	private static final long POST_AIRPLANE_MODE_WAITING_TIME_MS = 30 * 1000;
	
	// Dynamics------------------------------------------------------
	private SignalMonitor gsmMonitor;
	private List<DropboxSync> folderObservers;
	private SMSSender smsSender;
	private boolean isSending;
	private int startMode; // indicates how to behave if the service is killed
	private boolean allowRebind; // indicates whether onRebind should be used
	private String databasePath;
	private DataAccess dao;
	private Logger logger;
	
	private ScheduledExecutorService scheduleTaskExecutor;
	private ScheduledFuture<?> mScheduledFuture;

	@Override
	public void onCreate()
	{
		// Set the variable to false
		isSending = false;

		// Creates a thread pool that can schedule commands to run after a given
		// delay, or to execute periodically.
		scheduleTaskExecutor = Executors.newScheduledThreadPool(1);

		// Db path:
		databasePath = getFilesDir().getAbsolutePath();
		
		// DataAccess instance:
		dao = DataAccess.getInstance(databasePath);
		
		// Folder observers:
		folderObservers = new ArrayList<DropboxSync>();
		
		// Wait for the Debugger to be attached
		// android.os.Debug.waitForDebugger();
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		// Get the preferences
		final int timeSchedule = DataSenderPreferences.getTimeSchedule(this);
		
		setServiceForeground(this);
		
		boolean smsUpload = false;
		for(Project p : dao.retrieveProjects())
		{
			if(p.isLogging())
			{
				try
				{
					logger = new Logger(p.getLogFolderPath(), LOG_PREFIX);
					logger.addLine("PROJECT_SEND", p.getName());
					logger.addBlankLine();
				}
				catch(IOException e)
				{
					Log.e(TAG, "Logger construction error", e);
				}
			}

			Settings settings = p.getTransmissionSettings(); 
			// Upload via SMS
			if(settings.isSMSUpload())
			{
				smsUpload = true;
				if(!settings.isSMSIntroductionSent())
				{	//TODO send introduction
					
				}
			}
			
			// Upload to Dropbox
			if(settings.isDropboxUpload())
			{
				try
				{
					DropboxSync observer = new DropboxSync(getApplicationContext(), p.getDataFolder()); 
					folderObservers.add(observer);
					observer.startWatching();
				}
				catch(Exception e)
				{
					Log.w(TAG, "Could not set up dropbox observer for project " + p.getName());
				}
			}
		}
		
		//if at least one project needs SMS sending:
		if(smsUpload)
			smsSender = new SMSSender(this);
		
		//Start GSM SignalMonitor
		gsmMonitor = new SignalMonitor(this);
		
		// ================================================================================
		// Schedule Transmitting
		// ================================================================================
		// Check if the scheduleTaskExecutor is running and stop it first
		if(isSending)
		{
			mScheduledFuture.cancel(true);
			isSending = false;
		}
		// This schedule a runnable task every TIME_SCHEDULE in minutes
		mScheduledFuture = scheduleTaskExecutor.scheduleAtFixedRate(new SendingTask(), 0, timeSchedule, TimeUnit.MINUTES);

		return startMode;
	}

	@SuppressWarnings("deprecation")
	public void setServiceForeground(Context mContext)
	{
		final int myID = 9999;

		// The intent to launch when the user clicks the expanded notification
		Intent mIntent = new Intent(mContext, DataSenderPreferences.class);
		mIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_SINGLE_TOP);
		PendingIntent pendIntent = PendingIntent.getActivity(mContext, 0, mIntent, 0);

		// This constructor is deprecated. Use Notification.Builder instead
		Notification mNotification = new Notification(R.drawable.sender, getString(R.string.title_activity_main), System.currentTimeMillis());

		// This method is deprecated. Use Notification.Builder instead.
		mNotification.setLatestEventInfo(this, getString(R.string.title_activity_main), getString(R.string.notification), pendIntent);

		mNotification.flags |= Notification.FLAG_NO_CLEAR;
		startForeground(myID, mNotification);
	}

	@Override
	public IBinder onBind(Intent intent)
	{
		return null;
	}

	@Override
	public boolean onUnbind(Intent intent)
	{
		// All clients have unbound with unbindService()
		return allowRebind;
	}

	@Override
	public void onRebind(Intent intent)
	{
		// A client is binding to the service with bindService(),
		// after onUnbind() has already been called
	}

	@Override
	public void onDestroy()
	{
		// Go to AirplaneMode if needed:
		if(DataSenderPreferences.getAirplaneMode(this) && !DeviceControl.inAirplaneMode(this))
			DeviceControl.toggleAirplaneMode(this);
		// The service is no longer used and is being destroyed
		mScheduledFuture.cancel(true);
		stopSelf();
		int pid = android.os.Process.myPid();
		if(Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "BackgroundService: onDestroy() + killProcess(" + pid + ") ");
		android.os.Process.killProcess(pid);
	}
	
	private class SendingTask implements Runnable
	{
		
		private Context context = DataSenderService.this;
		
		public void run()
		{
			//Come out of airplane more if needed
			if(DataSenderPreferences.getAirplaneMode(context) && DeviceControl.inAirplaneMode(context))
			{
				DeviceControl.toggleAirplaneMode(context);
				
				//Wait for connectivity to become available
				try
				{	
					Thread.sleep(POST_AIRPLANE_MODE_WAITING_TIME_MS);
				}
				catch(Exception ignore) {}
			}
			
			//Generate transmissions...
			for(Project p : dao.retrieveProjects())
			{
				for(Form f : p.getForms())
				{	
					Schema schema = f.getSchema();
					List<Record> records = dao.retrieveRecordsWithoutTransmission(schema);
				
					//Decide on transmission mode
					
					
					BinarySMSTransmission t = null;
					
					while(!records.isEmpty() && !t.isFull())
					{
						try
						{
							t.addRecord(records.get(0));
							
							records.remove(0);
						}
						catch(Exception e)
						{
							e.printStackTrace();
						}
						
					}
					
					for(Record r : dao.retrieveRecordsWithoutTransmission(schema))
					{
						//BinarySMSTransmission transm = new BinarySMSTransmission(schema, 0, null, null);
						
					}
					
				}
				
			}
			
			int tempCount = 0;
			while(!gsmMonitor.isInService() && tempCount < DataSenderPreferences.getMaxAttempts(DataSenderService.this))
			{
				Log.i(Constants.TAG, "Connection Attempt! " + tempCount);
				// Wait for 1 a second on every attempt
				try
				{
					Thread.sleep(1000);
				}
				catch(InterruptedException e)
				{
					if(Constants.DEBUG_LOG)
						Log.i(Constants.TAG, "signalCheck() error: " + e.toString());
				}
				tempCount++;
			}
			
			isSending = true;

			//if(Constants.DEBUG_LOG)
			//	Log.i(Constants.TAG, "---------------------- Run Every: " + timeSchedule + " minutes!!!! --------------------------");
			
			//Go back to airplane more if needed
			if(DataSenderPreferences.getAirplaneMode(context) && !DeviceControl.inAirplaneMode(context))
				DeviceControl.toggleAirplaneMode(context);
		}
	}

}