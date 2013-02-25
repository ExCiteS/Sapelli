package uk.ac.excites.sender;

import java.io.File;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import uk.ac.excites.sender.dropbox.Dropbox;
import android.app.Notification;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.Environment;
import android.os.FileObserver;
import android.os.IBinder;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.TelephonyManager;
import android.util.Log;

import com.dropbox.sync.android.DbxAccountManager;
import com.dropbox.sync.android.DbxFile;
import com.dropbox.sync.android.DbxFileSystem;
import com.dropbox.sync.android.DbxPath;

/**
 * A service that checks about network connectivity and tries to send the data Also the service tries to upload file attachments to Dropbox
 * 
 * @author Michalis Vitos
 * 
 */
public class SenderBackgroundService extends Service
{
	// ================================================================================
	// Static Fields
	// ================================================================================
	// TODO set up the folder to observe
	private static final String pathToObserve = Environment.getExternalStorageDirectory().toString() + File.separator + "000sample" + File.separator;
	private static boolean DROPBOX_UPLOAD;
	private static boolean AIRPLANE_MODE;
	private static String CENTER_PHONE_NUMBER;
	private static int TIME_SCHEDULE;
	private static int MAX_ATTEMPS;
	private static DropboxSync folderObserver;

	// ================================================================================
	// Define some variables
	// ================================================================================
	private Context mContext;
	private int mStartMode; // indicates how to behave if the service is killed
	private boolean mAllowRebind; // indicates whether onRebind should be used
	private static int serviceState = -1;

	private ScheduledExecutorService scheduleTaskExecutor;
	private ScheduledFuture<?> mScheduledFuture;

	@Override
	public void onCreate()
	{
		// The service is being created
		this.mContext = this;

		checkServiceListener();

		// Wait for the Debugger to be attached
		// android.os.Debug.waitForDebugger();
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{

		// Get the preferences
		DROPBOX_UPLOAD = Preferences.getDropboxUpload(mContext);
		AIRPLANE_MODE = Preferences.getAirplaneMode(mContext);
		CENTER_PHONE_NUMBER = Preferences.getCenterPhoneNumber(mContext);
		TIME_SCHEDULE = Preferences.getTimeSchedule(mContext);
		MAX_ATTEMPS = Preferences.getMaxAttemps(mContext);

		setServiceForeground(mContext);

		// ================================================================================
		// Upload to Dropbox
		// ================================================================================
		if(DROPBOX_UPLOAD)
		{
			// TODO check if is logged in to dropbox
			Log.i(Constants.TAG, "--- Setting up observer for folder : " + pathToObserve);
			folderObserver = new DropboxSync(pathToObserve);
			folderObserver.startWatching();
		}

		// ================================================================================
		// Schedule SMS Transmitting
		// ================================================================================
		scheduleTaskExecutor = Executors.newScheduledThreadPool(1);

		// This schedule a runnable task every TIME_SCHEDULE in minutes
		mScheduledFuture = scheduleTaskExecutor.scheduleAtFixedRate(new Runnable()
		{
			public void run()
			{

				if(Constants.DEBUG_LOG)
					Log.i(Constants.TAG, "---------------------- Run Every: " + TIME_SCHEDULE + " minutes!!!! --------------------------");

				// If the phone is in AirplaneMode, set it to off
				// Wait until the phone is connected
				if(Utilities.inAirplaneMode(mContext))
				{
					if(Constants.DEBUG_LOG)
						Log.i(Constants.TAG, "The device is in Airplane mode");

					Utilities.toggleAirplaneMode(mContext);

					int tempCount = 0;

					while(serviceState != 0 && tempCount < MAX_ATTEMPS)
					{

						Log.i(Constants.TAG, "Connection Attemp! " + tempCount);

						// Wait for 1 a second on every attempt
						try
						{
							Thread.sleep(1000);
						}
						catch(InterruptedException e)
						{
							if(Constants.DEBUG_LOG)
								Log.i(Constants.TAG, e.toString());
						}
						tempCount++;
					}
				}

				if(serviceState == 0)
				{

					// TODO Send messages
					for(int i = 0; i < 10; i++)
					{
						Log.i(Constants.TAG, "SENDING MESSAGE!!!! " + i);
						try
						{
							Thread.sleep(1000);
						}
						catch(InterruptedException e)
						{
							e.printStackTrace();
						}
					}
				}

				// Get in the AirplaneMode only if it is checked in the settings
				Log.i(Constants.TAG, "The device is" + (AIRPLANE_MODE == true ? "" : " not") + " getting to Airplane Mode.");
				if(!Utilities.inAirplaneMode(mContext) && (AIRPLANE_MODE == true))
					Utilities.toggleAirplaneMode(mContext);
			}
		}, 0, TIME_SCHEDULE, TimeUnit.MINUTES);

		return mStartMode;
	}

	/**
	 * Check if there is GSM connectivity. The serrviceState has 3 modes, 0 : Normal operation condition, the phone is registered with an operator either in
	 * home network or in roaming. 1 : Phone is not registered with any operator, the phone can be currently searching a new operator to register to, or not
	 * searching to registration at all, or registration is denied, or radio signal is not available. 3 : Radio of telephony is explicitly powered off.
	 */
	public void checkServiceListener()
	{

		TelephonyManager telephonyManager = (TelephonyManager) mContext.getSystemService(Context.TELEPHONY_SERVICE);

		PhoneStateListener serviceListener = new PhoneStateListener()
		{
			public void onServiceStateChanged(ServiceState service)
			{

				serviceState = service.getState();
				if(Constants.DEBUG_LOG)
					Log.i(Constants.TAG, "GSM Service state: " + serviceState);

			}
		};

		telephonyManager.listen(serviceListener, PhoneStateListener.LISTEN_SERVICE_STATE);
	}

	@SuppressWarnings("deprecation")
	public void setServiceForeground(Context mContext)
	{

		final int myID = 9999;

		// The intent to launch when the user clicks the expanded notification
		Intent mIntent = new Intent(mContext, SenderBackgroundActivity.class);
		mIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_SINGLE_TOP);
		PendingIntent pendIntent = PendingIntent.getActivity(mContext, 0, mIntent, 0);

		// This constructor is deprecated. Use Notification.Builder instead
		Notification mNotification = new Notification(R.drawable.ic_launcher, getString(R.string.title_activity_main), System.currentTimeMillis());

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
		return mAllowRebind;
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
		if(!Utilities.inAirplaneMode(mContext))
			Utilities.toggleAirplaneMode(mContext);
		// The service is no longer used and is being destroyed
		mScheduledFuture.cancel(true);
		stopSelf();
		int pid = android.os.Process.myPid();
		if(Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "BackgroundService: onDestroy() + killProcess(" + pid + ") ");
		android.os.Process.killProcess(pid);
	}

	/**
	 * Class to define the Folder Sync and Upload to Dropbox
	 * 
	 * @author Michalis Vitos
	 * 
	 */
	public class DropboxSync extends FileObserver
	{
		private static final int flags = FileObserver.CREATE | FileObserver.DELETE;
		private String absolutePath;

		// Dropbox Variables
		private DbxAccountManager mDbxAcctMgr;
		private DbxFileSystem dbxFs;

		/**
		 * Constructor takes the Folder to watch as a parameter
		 * 
		 * @param path
		 */
		public DropboxSync(String path)
		{
			super(path, flags);
			absolutePath = path;

			// Setup Dropbox
			try
			{
				Dropbox mDropbox = new Dropbox(getApplicationContext());

				if(mDropbox.hasLinkedAccount())
				{
					// Set up Dropbox
					mDbxAcctMgr = mDropbox.getDropboxManager();
					dbxFs = DbxFileSystem.forAccount(mDbxAcctMgr.getLinkedAccount());
				}
				else
				{
					mDropbox.linkAccount();
				}
			}
			catch(Exception e)
			{
				Log.e(Constants.TAG, "DropboxSync() error: " + e.toString());
			}
		}

		@Override
		public synchronized void onEvent(int event, String path)
		{

			// Make sure the path is not null
			if(path == null)
			{
				return;
			}

			// File to upload to Dropbox
			File fileToUpload = new File(absolutePath + path);

			// Check what changed to the Projects Folder and upload or delete the file
			switch(event)
			{
			case FileObserver.CREATE:
				Log.i(Constants.TAG, "File: " + fileToUpload + " was created.");
				uploadFile(fileToUpload);
				break;

			case FileObserver.DELETE:
				deleteFile(fileToUpload);
				// Log.i(MainActivity.TAG, "File: " + fileToUpload + " was deleted.");
				break;
			}
		}

		public void uploadFile(File fileToUpload)
		{
			DbxFile dropboxFile = null;
			try
			{
				// Path to the Dropbox Structure where to upload the file
				// TODO Add the Project's Folder etc
				DbxPath dropboxPath = new DbxPath(fileToUpload.getName());
				Log.i(Constants.TAG, "File does " + (!dbxFs.isFile(dropboxPath) ? "not " : "") + "exist");
				if(dbxFs.isFile(dropboxPath))
					dropboxFile = dbxFs.open(dropboxPath);
				else
					dropboxFile = dbxFs.create(dropboxPath);

				// Upload the file to Dropbox
				dropboxFile.writeFromExistingFile(fileToUpload, false);
			}
			catch(Exception e)
			{
				Log.e(Constants.TAG, e.toString());
			}
			finally
			{
				if(dropboxFile != null)
					dropboxFile.close();
				Log.i(Constants.TAG, "File upload scheduled: " + fileToUpload.getName());
			}
		}

		public void deleteFile(File fileToDelete)
		{
			// For now, do not do anything
		};
	}
}