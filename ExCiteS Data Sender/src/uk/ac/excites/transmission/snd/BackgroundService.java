package uk.ac.excites.transmission.snd;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import android.app.AlarmManager;
import android.app.Notification;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.IBinder;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.TelephonyManager;
import android.util.Log;

/**
 * A service that checks which application is on the top of the activity stack. If not run the Launcher application.
 * 
 * @author Michalis Vitos
 * 
 */
public class BackgroundService extends Service
{

	// Define some variables
	private Context mContext;
	private int mStartMode; // indicates how to behave if the service is killed
	private boolean mAllowRebind; // indicates whether onRebind should be used
	private static int serviceState = -1;

	ScheduledExecutorService scheduleTaskExecutor;
	ScheduledFuture<?> mScheduledFuture;

	AlarmManager mAlarmManager;

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
		// The service is starting, due to a call to startService()

		setServiceForeground(mContext);

		scheduleTaskExecutor = Executors.newScheduledThreadPool(1);

		// This schedule a runnable task every 3 seconds
		mScheduledFuture = scheduleTaskExecutor.scheduleAtFixedRate(new Runnable()
		{
			public void run()
			{

				if (Constants.DEBUG_LOG)
					Log.i(Constants.TAG, "---------------------- Run Every: " + Preferences.getTimeSchedule(mContext) + " minutes!!!! --------------------------");

				// If the phone is in AirplaneMode, set it to off
				// Wait until the phone is connected
				if (Utilities.inAirplaneMode(mContext))
				{

					Utilities.toggleAirplaneMode(mContext);

					int tempCount = 0;

					while (serviceState != 0 && tempCount < Preferences.getMaxAttemps(mContext))
					{

						Log.i(Constants.TAG, "Connection Attemp! " + tempCount);

						// Wait for 1 a second
						try
						{
							Thread.sleep(1000);
						} catch (InterruptedException e)
						{
							e.printStackTrace();
						}
						tempCount++;
					}
				}

				if (serviceState == 0)
				{

					// TODO Send messages
					for (int i = 0; i < 10; i++)
					{
						Log.i(Constants.TAG, "SENDING MESSAGE!!!! " + i);
						try
						{
							Thread.sleep(1000);
						} catch (InterruptedException e)
						{
							e.printStackTrace();
						}
					}
				}

				// Get in the AirplaneMode
				if (!Utilities.inAirplaneMode(mContext))
					Utilities.toggleAirplaneMode(mContext);
			}
		}, 0, Preferences.getTimeSchedule(mContext), TimeUnit.MINUTES);

		return mStartMode;
	}

	/**
	 * Check if there is GSM connectivity. The serrviceState has 3 modes, 0 : Normal operation condition, the phone is registered with an operator either in home network or in roaming. 1 : Phone is not registered with any operator, the phone can be currently searching a new operator to register to,
	 * or not searching to registration at all, or registration is denied, or radio signal is not available. 3 : Radio of telephony is explicitly powered off.
	 */
	public void checkServiceListener()
	{

		TelephonyManager telephonyManager = (TelephonyManager) mContext.getSystemService(Context.TELEPHONY_SERVICE);

		PhoneStateListener serviceListener = new PhoneStateListener()
		{
			public void onServiceStateChanged(ServiceState service)
			{

				serviceState = service.getState();
				if (Constants.DEBUG_LOG)
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
		Intent mIntent = new Intent(mContext, BackgroundActivity.class);
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
		if (!Utilities.inAirplaneMode(mContext))
			Utilities.toggleAirplaneMode(mContext);
		// The service is no longer used and is being destroyed
		mScheduledFuture.cancel(true);
		stopSelf();
		int pid = android.os.Process.myPid();
		if (Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "BackgroundService: onDestroy() + killProcess(" + pid + ") ");
		android.os.Process.killProcess(pid);
	}
}