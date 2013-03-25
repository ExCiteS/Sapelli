package uk.ac.ucl.excites.sender.util;

import uk.ac.ucl.excites.sender.DataSenderService;
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
		if(DataSenderService.class.getName() != null && ServiceChecker.isMyServiceRunning(context.getApplicationContext(), DataSenderService.class.getName()))
		{
			// Call the Service
			Intent intent = new Intent(context, DataSenderService.class);
			context.startService(intent);
		}
	}
}
