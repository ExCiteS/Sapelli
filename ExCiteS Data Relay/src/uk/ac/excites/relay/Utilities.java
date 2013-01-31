package uk.ac.excites.relay;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningServiceInfo;
import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.provider.Settings;
import android.util.Log;

/**
 * This class contains various utilities methods
 * 
 * @author Michalis Vitos
 * 
 */
public class Utilities extends Application
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
		ActivityManager manager = (ActivityManager) mContext.getSystemService(ACTIVITY_SERVICE);
		for (RunningServiceInfo service : manager.getRunningServices(Integer.MAX_VALUE))
		{

			// Set the Service
			if (serviceClassName.equals(service.service.getClassName()))
			{
				return true;
			}
		}
		return false;
	}

	/**
	 * Check to see if the phone is in AiroplaneMode
	 * 
	 * @return true if it is
	 */
	public static boolean inAirplaneMode(Context mContext)
	{
		return Settings.System.getInt(mContext.getContentResolver(), Settings.System.AIRPLANE_MODE_ON, 0) == 1;
	}

	/**
	 * Toggle thought the AirplaneMode
	 */
	public static void toggleAirplaneMode(Context mContext)
	{

		boolean isInAirplaneMode = inAirplaneMode(mContext);

		try
		{
			// If airplane mode is on, value 0, else value is 1
			Settings.System.putInt(mContext.getContentResolver(), Settings.System.AIRPLANE_MODE_ON, isInAirplaneMode ? 0 : 1);

			// Reload when the mode is changed each time by sending Intent
			Intent intent = new Intent(Intent.ACTION_AIRPLANE_MODE_CHANGED);
			intent.putExtra("state", !isInAirplaneMode);
			mContext.sendBroadcast(intent);

			if (Constants.DEBUG_LOG)
				Log.i(Constants.TAG, "Airplane mode is: " + (isInAirplaneMode ? "OFF" : "ON"));

		} catch (Exception e)
		{
			if (Constants.DEBUG_LOG)
				Log.i(Constants.TAG, "exception:" + e.toString());
		}
	}

	/**
	 * Check if the web server is up and running
	 * @param mContext
	 * @return
	 */
	public static boolean isServerOnline(Context mContext)
	{

		if (isOnline(mContext))
		{
			try
			{
				String text = "";
				URL url = new URL(Preferences.getServerAddress(mContext));
				URLConnection mURLConnection = url.openConnection();
				BufferedReader mBufferedReader = new BufferedReader(new InputStreamReader(mURLConnection.getInputStream()));
				String line;
				while ((line = mBufferedReader.readLine()) != null)
				{
					text = text + line + "\n";
				}
				mBufferedReader.close();
				if (text.contains("HTTP_CONNECTIVITY_OK"))
					return true;
			} catch (Exception e)
			{
				return false;
			}
		}
		return false;
	}

	/**
	 * Check if the device is connected to Internet
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean isOnline(Context mContext)
	{
		ConnectivityManager cm = (ConnectivityManager) mContext.getSystemService(Context.CONNECTIVITY_SERVICE);
		NetworkInfo netInfo = cm.getActiveNetworkInfo();
		if (netInfo != null && netInfo.isConnected())
		{
			return true;
		}
		return false;
	}
}
