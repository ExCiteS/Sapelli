package uk.ac.ucl.excites.sapelli.transmission.sender.util;

import uk.ac.ucl.excites.sapelli.transmission.sender.RecordSenderService;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.ActivityManager;
import android.app.ActivityManager.RunningServiceInfo;
import android.content.Context;
import android.content.Intent;

/**
 * This class contains various utilities methods
 * 
 * @author Michalis Vitos
 * 
 */
public class ServiceChecker
{

	public static final String DATA_SENDER_SERVICE = RecordSenderService.class.getName();

	/**
	 * Check if a Service is Running
	 * 
	 * @param context
	 * @param serviceClassName
	 *            ex. <code>uk.ac.ucl.excites.launcher.LauncherService</code>
	 * @return
	 */
	public static boolean isMyServiceRunning(Context context, String serviceClassName)
	{
		ActivityManager manager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
		for(RunningServiceInfo service : manager.getRunningServices(Integer.MAX_VALUE))
		{
			// Set the Service
			if(serviceClassName != null && serviceClassName.equals(service.service.getClassName()))
				return true;
		}
		return false;
	}

	/**
	 * Method to check if the DataSenderService is Running and restart it
	 */
	public static void restartActiveDataSender(Context context)
	{
		if(DATA_SENDER_SERVICE != null && isMyServiceRunning(context.getApplicationContext(), DATA_SENDER_SERVICE))
		{
			// Call the Service
			Intent intent = new Intent(context, RecordSenderService.class);
			context.startService(intent);
		}
	}

	/**
	 * Method to start the Service if it is not already active
	 */
	public static void startService(Context context)
	{
		if(DATA_SENDER_SERVICE != null && !isMyServiceRunning(context.getApplicationContext(), DATA_SENDER_SERVICE))
		{
			// Call the Service
			Intent intent = new Intent(context, RecordSenderService.class);
			context.startService(intent);
		}
	}

	/**
	 * Method to stop the service if it is already active
	 * 
	 * @param context
	 */
	public static void stopService(Context context)
	{
		while(isMyServiceRunning(context, DATA_SENDER_SERVICE))
		{
			// Terminate the service
			Intent intent = new Intent(context, RecordSenderService.class);
			if(context.stopService(intent))
				Debug.d("Service Stoped");
			else
				Debug.d("Cannot Stop the Service.");
		}
	}
}
