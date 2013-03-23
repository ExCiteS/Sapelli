package uk.ac.ucl.excites.sender;

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
import uk.ac.ucl.excites.sender.util.Constants;
import uk.ac.ucl.excites.sender.util.ServiceChecker;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.util.DeviceControl;
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
	
	// Dynamics------------------------------------------------------
	private List<DropboxSync> folderObservers;
	private SMSSender smsSender;
	private boolean isSending;
	private int startMode; // indicates how to behave if the service is killed
	private boolean allowRebind; // indicates whether onRebind should be used
	private String databasePath;
	private DataAccess dao;
	
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
//		AIRPLANE_MODE = DataSenderPreferences.getAirplaneMode(this);
		final int timeSchedule = DataSenderPreferences.getTimeSchedule(this);
		
		setServiceForeground(this);
		
		boolean smsUpload = false;
		for(Project p : dao.retrieveProjects())
		{
			Settings settings = p.getTransmissionSettings(); 
			// Upload via SMS
			if(settings.isSMSUpload())
			{
				smsUpload = true;
				if(!settings.isSMSIntroductionSent())
				{	//TODO sent introduction
					
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
			smsSender = new SMSSender(this, DataSenderPreferences.getMaxAttempts(this));
		
		
		
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
		mScheduledFuture = scheduleTaskExecutor.scheduleAtFixedRate(new Runnable()
		{
			public void run()
			{
				
				
				for(Project p : dao.retrieveProjects())
				{
					for(Form f : p.getForms())
					{
						Schema schema = f.getSchema();
						
						List<Record> records = dao.retrieveRecordsWithoutTransmission(schema);
						
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
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
							
						}
						
						for(Record r : dao.retrieveRecordsWithoutTransmission(schema))
						{
							//BinarySMSTransmission transm = new BinarySMSTransmission(schema, 0, null, null);
							
							
							
						}
						
					}
					
					
					
				}
				
				
				
				
				isSending = true;

				if(Constants.DEBUG_LOG)
					Log.i(Constants.TAG, "---------------------- Run Every: " + timeSchedule + " minutes!!!! --------------------------");
			}
		}, 0, timeSchedule, TimeUnit.MINUTES);

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
		// Get in the AirplaneMode
		if(!DeviceControl.inAirplaneMode(this))
			DeviceControl.toggleAirplaneMode(this);
		// The service is no longer used and is being destroyed
		mScheduledFuture.cancel(true);
		stopSelf();
		int pid = android.os.Process.myPid();
		if(Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "BackgroundService: onDestroy() + killProcess(" + pid + ") ");
		android.os.Process.killProcess(pid);
	}

}