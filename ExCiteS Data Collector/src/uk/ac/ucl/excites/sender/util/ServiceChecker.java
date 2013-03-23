package uk.ac.ucl.excites.sender.util;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningServiceInfo;
import android.content.Context;

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
	 * @param mContext
	 * @param serviceClassName
	 *            ex. uk.ac.ucl.excites.launcher.LauncherService
	 * @return
	 */
	public static boolean isMyServiceRunning(Context mContext, String serviceClassName)
	{
		ActivityManager manager = (ActivityManager) mContext.getSystemService(Context.ACTIVITY_SERVICE);
		for(RunningServiceInfo service : manager.getRunningServices(Integer.MAX_VALUE))
		{
			// Set the Service
			if(serviceClassName != null && serviceClassName.equals(service.service.getClassName()))
			{
				return true;
			}
		}
		return false;
	}

}
