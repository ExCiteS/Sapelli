package uk.ac.ucl.excites.sender;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
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
import uk.ac.ucl.excites.collector.database.DataAccess;
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
import uk.ac.ucl.excites.transmission.TransmissionSender;
import uk.ac.ucl.excites.transmission.http.HTTPClient;
import uk.ac.ucl.excites.transmission.sms.SMSService;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.sms.SMSTransmissionID;
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
public class DataSenderService extends Service implements TransmissionSender
{

	// Statics-------------------------------------------------------
	static private final String TAG = "DataSenderService";
	static private final String LOG_PREFIX = "Sender_";
	private static final long POST_AIRPLANE_MODE_WAITING_TIME_MS = 30 * 1000;
	private static final long PRE_AIRPLANE_MODE_WAITING_TIME_MS = 30 * 1000;
	
	// Dynamics------------------------------------------------------
	private SignalMonitor gsmMonitor;
	private List<DropboxSync> folderObservers;
	private SMSSender smsSender;
	private SMSTransmissionID smsTransmissionID;
	private boolean isSending;
	private int startMode = START_STICKY; // indicates how to behave if the service is killed
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
		
		// ID generator for SMSTransmissions:
		smsTransmissionID = dao.retrieveTransmissionID();
		if(smsTransmissionID == null)
		{
			smsTransmissionID = new SMSTransmissionID();
			dao.store(smsTransmissionID);
		}
		
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
		boolean dropboxUpload = DataSenderPreferences.getDropboxUpload(this);
		boolean smsUpload = DataSenderPreferences.getSMSUpload(this);
		
		setServiceForeground(this);
		
		boolean projectWithSMSEnabled = false;

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
			if(smsUpload && settings.isSMSUpload())
			{
				projectWithSMSEnabled = true;
				if(!settings.isSMSIntroductionSent())
				{	//TODO send introduction
					
				}
			}
			
			// Upload to Dropbox
			if(dropboxUpload && settings.isDropboxUpload())
			{
				try
				{
					DropboxSync observer = new DropboxSync(getApplicationContext(), p.getDataFolder(), ((CollectorApp) getApplication()).getExcitesFolderPath()); 
					folderObservers.add(observer);
					observer.startWatching();
				}
				catch(Exception e)
				{
					Debug.d("Could not set up Dropbox Observer for project " + p.getName());
				}
			}
		}

		//if at least one project needs SMS sending:
		if(projectWithSMSEnabled)
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
	}
	
	private class SendingTask implements Runnable
	{
		private Context context = DataSenderService.this;
		
		public void run()
		{
			// Try to save any exception into the SD Card
			try
			{
				Debug.d("-- SendingTask Started --");

				for(Entry<Project, Logger> pl : loggers.entrySet())
					pl.getValue().addLine("Sending task");
				
				//Come out of airplane more if needed
				if(DataSenderPreferences.getAirplaneMode(context) && DeviceControl.inAirplaneMode(context))
				{
					DeviceControl.toggleAirplaneMode(context);
					Debug.d("Phone was in AirplaneMode and try to get it out.");

					//Wait for connectivity to become available
					try
					{	
						Thread.sleep(POST_AIRPLANE_MODE_WAITING_TIME_MS);
					}
					catch(Exception ignore)
					{
						Debug.e(ignore);
					}
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
					
					//Log signal strength & roaming:
					loggers.get(p).addLine("Current cellular signal strength: " + gsmMonitor.getSignalStrength());
					loggers.get(p).addLine("Device is " + (gsmMonitor.isRoaming() ? "" : "not ") + "currently roaming.");
					
					Debug.d("Device is " + (gsmMonitor.isRoaming() ? "" : "not ") + "currently roaming");
					
					for(Form f : p.getForms())
					{		
						Schema schema = f.getSchema();			
						
						List<Record> records = new ArrayList<Record>(dao.retrieveRecordsWithoutTransmission(schema));
						Debug.d("Found " + records.size() + " records without a transmission for form " + f.getName() + " of project " + p.getName() + " (version " + p.getVersion() + ").");
						
						if(records.isEmpty())
						{
							loggers.get(p).addLine("No records to send for form " + f.getName());
							continue;
						}
						else
							loggers.get(p).addLine(records.size() + " records to send for form " + f.getName());
						
						//Decide on transmission mode
						//TODO
						
						//HTTP over GPRS/EDGE/3G/4G or Wi-Fi
						//TODO HTTPTransmissions
						
						//SMS
						/*Log.d(TAG, "prefs sms upload: " + DataSenderPreferences.getSMSUpload(DataSenderService.this));
						Log.d(TAG, "project sms upload: " + settings.isSMSUpload());
						Log.d(TAG, "gsm service: " + gsmMonitor.isInService());
						Log.d(TAG, "proj allow roaming: " + settings.isSMSAllowRoaming());
						Log.d(TAG, "gsm service roaming: " + gsmMonitor.isRoaming());
						Log.d(TAG, "roaming decision: " + (settings.isSMSAllowRoaming() || !gsmMonitor.isRoaming()));*/
						
						if(DataSenderPreferences.getSMSUpload(DataSenderService.this) && settings.isSMSUpload() && gsmMonitor.isInService() && (settings.isSMSAllowRoaming() || !gsmMonitor.isRoaming()))
						{
							Log.d(TAG, "Attempting SMS transmission generation");
							
							//Generate transmission(s)
							List<SMSTransmission> smsTransmissions = generateSMSTransmissions(p, schema, records.iterator());
							
							//Store transmission(s) & update records so associated transmission is stored
							for(Transmission t : smsTransmissions)
							{
								Log.d(TAG, "Transmission " + ((SMSTransmission) t).getID());
								
								for(Record r : t.getRecords())
									dao.store(r);
								dao.store(t);
							}
							
							//TODO fetch unsent existing smstransmissions from db
							
							//Send transmission(s)
							//TODO check signal again?
							for(SMSTransmission t : smsTransmissions)
							{
								try
								{
									loggers.get(p).addLine("Sending SMSTransmission with ID " + t.getID() + ", containing " + t.getRecords().size() + " records (compression ratio " + (t.getCompressionRatio() * 100) + "%), stored in " + t.getTotalParts() + " messages");
									Log.d(TAG, "Trying to send SMSTransmission with ID " + t.getID() + ", containing " + t.getRecords().size() + " records (compression ratio " + (t.getCompressionRatio() * 100) + "%), stored in " + t.getTotalParts() + " messages");
									t.send(DataSenderService.this);
								}
								catch(Exception e)
								{
									loggers.get(p).addLine("Error upon sending SMSTransmission: " + e.getMessage());
									Log.e(TAG, "error on sending smstransmission", e);
								}
							}
						}
						
					}
					
					//commit changes to db:
					dao.commit();
				}
				
				isSending = true;
				
				//Go back to airplane more if needed
				if(DataSenderPreferences.getAirplaneMode(context) && !DeviceControl.inAirplaneMode(context))
				{
					try
					{	//Wait for messages to be sent
						Thread.sleep(PRE_AIRPLANE_MODE_WAITING_TIME_MS);
					}
					catch(Exception ignore)
					{
						Debug.e(ignore);
					}

					DeviceControl.toggleAirplaneMode(context);
					Debug.d("Phone must go in AirplaneMode and try to get it in.");
				}

				Debug.d("-- SendingTask Ended --");
			}
			catch(Exception e)
			{
				Debug.e(e);
				Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), e);
			}
		}
			
	}
	
	private List<SMSTransmission> generateSMSTransmissions(Project project, Schema schema, Iterator<Record> records)
	{
		//Settings:
		Settings settings = project.getTransmissionSettings();
		
		//Columns to factor out:
		Set<Column<?>> factorOut = new HashSet<Column<?>>();
		factorOut.add(schema.getColumn(Form.COLUMN_DEVICE_ID));
		
		//Make transmissions: 
		List<SMSTransmission> transmissions = new ArrayList<SMSTransmission>();
		while(records.hasNext())
		{
			// Create transmission:			
			SMSTransmission t = null;
			switch(settings.getSMSMode())
			{
				case BINARY : t = new BinarySMSTransmission(schema, factorOut, smsTransmissionID.getNewID(), settings.getSMSRelay(), settings); break;
				case TEXT : t = new TextSMSTransmission(schema, factorOut, smsTransmissionID.getNewID(), settings.getSMSRelay(), settings); break;
			}
			
			// Update SMSTransmissionID:
			dao.store(smsTransmissionID);
			
			// Add as many records as possible:
			while(records.hasNext() && !t.isFull())
			{
				Record r = records.next();
				try
				{
					t.addRecord(r);
				}
				catch(Exception e)
				{
					loggers.get(project).addLine("Error upon adding record: " + r.toString());
					Log.e(TAG, "Error upon adding record", e);
				}
			}
			if(!t.isEmpty())
				transmissions.add(t);
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

	@Override
	public SMSService getSMSService()
	{
		return smsSender;
	}

	@Override
	public HTTPClient getHTTPClient()
	{
		// TODO return HTTPClient
		return null;
	}

}