package uk.ac.ucl.excites.sender;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import uk.ac.ucl.excites.collector.CollectorApp;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.sender.dropbox.DropboxSync;
import uk.ac.ucl.excites.sender.gsm.SMSSender;
import uk.ac.ucl.excites.sender.gsm.SignalMonitor;
import uk.ac.ucl.excites.sender.util.Constants;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.transmission.sms.text.TextSMSTransmission;
import uk.ac.ucl.excites.util.Debug;
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
	static private final String LOG_PREFIX = "Sender_";
	private static final long POST_AIRPLANE_MODE_WAITING_TIME_MS = 30 * 1000;
	
	// Dynamics------------------------------------------------------
	private SignalMonitor gsmMonitor;
	private List<DropboxSync> folderObservers;
	private SMSSender smsSender;
	private boolean isSending;
	private int startMode; // indicates how to behave if the service is killed
	private boolean allowRebind; // indicates whether onRebind should be used
	private DataAccess dao;
	private Map<Project,Logger> loggers;
	
	private ScheduledExecutorService scheduleTaskExecutor;
	private ScheduledFuture<?> mScheduledFuture;

	@Override
	public void onCreate()
	{
		//Loggers
		loggers = new HashMap<Project, Logger>();
		
		// Set the variable to false
		isSending = false;

		// Creates a thread pool that can schedule commands to run after a given
		// delay, or to execute periodically.
		scheduleTaskExecutor = Executors.newScheduledThreadPool(1);

		// DataAccess instance:
		dao = ((CollectorApp) getApplication()).getDatabaseInstance();
		
		// Folder observers:
		folderObservers = new ArrayList<DropboxSync>();
		
		// Wait for the Debugger to be attached
		//android.os.Debug.waitForDebugger();
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		// Get the preferences
		final int timeSchedule = DataSenderPreferences.getTimeSchedule(this);
		final boolean dropboxUpload = DataSenderPreferences.getDropboxUpload(this);
		
		setServiceForeground(this);
		
		boolean smsUpload = false;

		for(Project p : dao.retrieveProjects())
		{
			if(p.isLogging())
			{
				try
				{
					Logger logger = new Logger(p.getLogFolderPath(), LOG_PREFIX);
					for(Entry<Project, Logger> pl : loggers.entrySet())
						pl.getValue().addLine("DataSender", "Service started.");
					loggers.put(p, logger);
				}
				catch(Exception e)
				{
					Debug.e("Logger construction error", e);
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
			if(dropboxUpload && settings.isDropboxUpload())
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
			smsSender = new SMSSender(this, dao);
		
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

	@Override
	public void onDestroy()
	{
		//Close loggers:
		for(Entry<Project, Logger> pl : loggers.entrySet())
			pl.getValue().addFinalLine("DataSender", "Service stopped.");
		
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

		// TODO Maybe here close the db
	}
	
	private class SendingTask implements Runnable
	{
		private Context context = DataSenderService.this;
		
		public void run()
		{
			Debug.d("----------------Runner!-------------");

			for(Entry<Project, Logger> pl : loggers.entrySet())
				pl.getValue().addLine("Sending task");
			
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
			
			//TODO Block until we have connectivity
			
//			int tempCount = 0;
//			while(!gsmMonitor.isInService() && tempCount < DataSenderPreferences.getMaxAttempts(DataSenderService.this))
//			{
//				Log.i(Constants.TAG, "Connection Attempt! " + tempCount);
//				// Wait for 1 a second on every attempt
//				try
//				{
//					Thread.sleep(1000);
//				}
//				catch(InterruptedException e)
//				{
//					if(Constants.DEBUG_LOG)
//						Log.i(Constants.TAG, "signalCheck() error: " + e.toString());
//				}
//				tempCount++;
//			}
			
			//Generate transmissions...
			for(Project p : dao.retrieveProjects())
			{
				Settings settings = p.getTransmissionSettings();
				
				for(Form f : p.getForms())
				{		
					Schema schema = f.getSchema();
					List<Record> records = new ArrayList<Record>(dao.retrieveRecordsWithoutTransmission(schema));
					
					Debug.d("Found " + records.size() + " records without a transmission for form " + f.getName() + " of project " + p.getName() + " (version "
							+ p.getVersion() + ").");
					for(Record record : records)
					{
						Debug.d("Record: " + record.toString());
					}

					//Decide on transmission mode
					if(settings.isSMSUpload() && gsmMonitor.isInService()) //TODO do roaming check
					{
						List<SMSTransmission> smsTransmissions = generateSMSTransmissions(settings, schema, records);
						for(Record r : records)
							dao.store(r); //update records so associated transmissions are stored
						dao.commit();
						
						//store transmissions
						for(Transmission t : smsTransmissions)
							dao.store(t);
						
						//TODO make sure they are restored upon sending call backs
						
						//TODO fetch unsent existing smstransmissions from db
						
						//Send them
						//TODO check signal again?
						for(SMSTransmission t : smsTransmissions)
						{
							try
							{
								Log.d(TAG, "Trying to send SMSTransmission with ID " + t.getID() + ", containing " + t.getRecords().size() + " records (compression ratio " + t.getCompressionRatio()*100 + "%), stored in " + t.getParts().size() + " messages");
								t.send(smsSender);
							}
							catch(Exception e)
							{
								//TODO
								Log.e(TAG, "error on sending smstransmission", e);
							}
						}
						
					}
				}
				
			}
			
			isSending = true;

			//if(Constants.DEBUG_LOG)
			//	Log.i(Constants.TAG, "---------------------- Run Every: " + timeSchedule + " minutes!!!! --------------------------");
			
			//Go back to airplane more if needed
			if(DataSenderPreferences.getAirplaneMode(context) && !DeviceControl.inAirplaneMode(context))
				DeviceControl.toggleAirplaneMode(context);
		}
	}
	
	private List<SMSTransmission> generateSMSTransmissions(Settings settings, Schema schema, List<Record> records)
	{
		//Columns to factor out:
		Set<Column<?>> factorOut = new HashSet<Column<?>>();
		factorOut.add(schema.getColumn(Form.COLUMN_DEVICE_ID));
		
		//Make transmissions: 
		List<SMSTransmission> transmissions = new ArrayList<SMSTransmission>();
		while(!records.isEmpty())
		{
			// Create transmission:			
			SMSTransmission t = null;
			switch(settings.getSMSMode())
			{
				case BINARY : t = new BinarySMSTransmission(schema, factorOut, settings.getSMSTransmissionID(), settings.getSMSRelay(), settings); break;
				case TEXT : t = new TextSMSTransmission(schema, factorOut, settings.getSMSTransmissionID(), settings.getSMSRelay(), settings); break;
			}
			//Add as many records as possible:
			while(!records.isEmpty() && !t.isFull())
			{
				try
				{
					t.addRecord(records.get(0));
				}
				catch(Exception e)
				{
					//TODO log to project logger
					Log.e(TAG, "Error upon adding record", e);
				}
				finally
				{
					records.remove(0);
				}
			}
			if(!t.isEmpty())
				transmissions.add(t);
			//TODO store setting to save next transmission ID !!
		}
		return transmissions;
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

}