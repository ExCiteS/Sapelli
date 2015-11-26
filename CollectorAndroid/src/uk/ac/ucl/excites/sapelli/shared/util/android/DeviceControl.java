/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.shared.util.android;

import java.io.File;

import android.annotation.TargetApi;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.media.AudioManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.BatteryManager;
import android.os.Build;
import android.os.Vibrator;
import android.provider.Settings;
import android.support.v4.content.ContextCompat;
import android.telephony.TelephonyManager;

/**
 * @author Michalis Vitos, mstevens
 * 
 */
public final class DeviceControl
{

	protected static final String TAG = "DeviceControl";
	protected static final String SAMSUNG_S7710_SD_PATH = "/storage/extSdCard";

	/**
	 * Check if the device is connected to Internet
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean isOnline(Context context)
	{
		return isOnline(((ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo());
	}
	
	public static boolean isWifiOnline(Context context)
	{
		return isOnline(((ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE)).getNetworkInfo(ConnectivityManager.TYPE_WIFI));
	}
	
	public static boolean isMobileOnline(Context context)
	{
		return isOnline(((ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE)).getNetworkInfo(ConnectivityManager.TYPE_MOBILE));
	}
	
	private static boolean isOnline(NetworkInfo netInfo)
	{
		return netInfo != null && netInfo.isConnected();
	}

	/**
	 * Number of seconds to wait after exiting the Airplane Mode for GSM to be connected
	 */
	public static final int POST_AIRPLANE_MODE_WAITING_TIME_S = 30;

	/**
	 * class should not be instantiated
	 */
	private DeviceControl() {}

	/**
	 * @param context
	 * @return one of the TelephoneManager#PHONE_TYPE_* values
	 */
	public static int getPhoneType(Context context)
	{
		TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
		return tm.getPhoneType();
	}

	public static boolean isGSM(Context context)
	{
		return getPhoneType(context) == TelephonyManager.PHONE_TYPE_GSM;
	}

	public static boolean isCDMA(Context context)
	{
		return getPhoneType(context) == TelephonyManager.PHONE_TYPE_CDMA;
	}

	public static String getSimCountryISOCode(Context context)
	{
		TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
		return tm.getSimCountryIso();
	}

	public static String getNetworkCountryISOCode(Context context)
	{
		TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
		return tm.getNetworkCountryIso();
	}

	/**
	 * Check to see if the phone is in AirplaneMode
	 * 
	 * @return true if it is
	 */
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
	@SuppressWarnings("deprecation")
	public static boolean inAirplaneMode(Context context)
	{
		if(Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1)
			return Settings.System.getInt(context.getContentResolver(), Settings.System.AIRPLANE_MODE_ON, 0) != 0;
		else
			return Settings.Global.getInt(context.getContentResolver(), Settings.Global.AIRPLANE_MODE_ON, 0) != 0;
	}

	/**
	 * Disable AirplaneMode and wait for {@code waitingSeconds} seconds for the device to exit
	 */
	public static void disableAirplaneModeAndWait(Context context, int waitingSeconds)
	{
		disableAirplaneMode(context);

		// Wait
		try
		{
			Thread.sleep(waitingSeconds * 1000);
		}
		catch(Exception e)
		{
			Debug.e(e);
		}
	}

	/**
	 * Disable AirplaneMode (Take device out of AirplaneMode)
	 */
	public static void disableAirplaneMode(Context context)
	{
		setAirplaneMode(context, false);
	}

	/**
	 * Enable AirplaneMode (Set the device in AirplaneMode)
	 */
	public static void enableAirplaneMode(Context context)
	{
		setAirplaneMode(context, true);
	}

	/**
	 * Set AirplaneMode
	 * 
	 * @param context
	 * @param enabled pass {@code true} to put the device in airplane mode, and {@code false} to leave airplane mode
	 */
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
	@SuppressWarnings("deprecation")
	private static void setAirplaneMode(Context context, boolean enabled)
	{
		try
		{
			// If airplane mode is on, value 0, else value is 1
			if(canSetAirplaneMode())
			{
				Settings.System.putInt(context.getContentResolver(), Settings.System.AIRPLANE_MODE_ON, enabled ? 1 : 0);

				// Reload when the mode is changed each time by sending Intent
				Intent intent = new Intent(Intent.ACTION_AIRPLANE_MODE_CHANGED);
				intent.putExtra("state", enabled);
				context.sendBroadcast(intent);

				Debug.d("Airplane mode is: " + (enabled ? "ON" : "OFF"));
			}
		}
		catch(Exception e)
		{
			Debug.e("Error upon toggling airplane more.", e);
		}
	}

	public static boolean canSetAirplaneMode()
	{
		return Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1;
	}

	public static void vibrate(Context context, int durationMS)
	{
		Vibrator vibrator = (Vibrator) context.getSystemService(Context.VIBRATOR_SERVICE);
		vibrator.vibrate(durationMS);
	}

	/**
	 * Returns the current battery level as a percentage.
	 * 
	 * @param context
	 * @return
	 */
	public static float getBatteryLevel(Context context)
	{
		Intent batteryIntent = context.registerReceiver(null, new IntentFilter(Intent.ACTION_BATTERY_CHANGED));
		int level = batteryIntent.getIntExtra(BatteryManager.EXTRA_LEVEL, -1);
		int scale = batteryIntent.getIntExtra(BatteryManager.EXTRA_SCALE, -1);

		// Error checking just in case.
		if(level == -1 || scale == -1)
		{
			return 50.0f;
		}

		return ((float) level / (float) scale) * 100.0f;
	}

	/**
	 * Increases the Media Volume
	 * 
	 * @param context
	 */
	public static void increaseMediaVolume(Context context)
	{
		// Get the AudioManager
		final AudioManager audioManager = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
		// Set the volume
		audioManager.adjustStreamVolume(AudioManager.STREAM_MUSIC, AudioManager.ADJUST_RAISE, 0);
	}

	/**
	 * Decreases the Media Volume
	 * 
	 * @param context
	 */
	public static void decreaseMediaVolume(Context context)
	{
		// Get the AudioManager
		final AudioManager audioManager = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
		// Set the volume
		audioManager.adjustStreamVolume(AudioManager.STREAM_MUSIC, AudioManager.ADJUST_LOWER, 0);
	}

	/**
	 * Decreases the Media Volume only up to half of the device's max volume
	 * 
	 * @param context
	 */
	public static void safeDecreaseMediaVolume(Context context)
	{
		// Get the volume
		final AudioManager audioManager = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
		final int currentVolume = audioManager.getStreamVolume(AudioManager.STREAM_MUSIC);
		final int maxVolume = audioManager.getStreamMaxVolume(AudioManager.STREAM_MUSIC);
		final int minVolume = maxVolume / 2;

		Debug.d("currentVolume: " + currentVolume);
		Debug.d("maxVolume: " + maxVolume);
		Debug.d("minVolume: " + minVolume);

		if(currentVolume > minVolume)
			decreaseMediaVolume(context);
	}

	/**
	 * Returns absolute paths to application-specific directories on all external storage devices where the application can place persistent files it owns as in
	 * {@link ContextCompat#getExternalFilesDirs(Context, String)}. This method also hard-codes the SD Card path for devices that are not supported in Android
	 * i.e. Samsung Xcover 2
	 * 
	 * @param context
	 * @param type
	 * @return
	 */
	public static File[] getExternalFilesDirs(Context context, String type)
	{
		// Get the paths ContextCompat
		File[] paths = ContextCompat.getExternalFilesDirs(context, type);

		String manufacturer = android.os.Build.MANUFACTURER;
		String model = android.os.Build.MODEL;

		// Check if Device is Samsung GT-S7710 (a.k.a. Samsung Galaxy Xcover 2)
		if(compare(manufacturer, "Samsung") && compare(model, "GT-S7710"))
		{
			// Hard code the path of the external SD Card
			paths = addPath(paths, getSdCardFilesDir(context, SAMSUNG_S7710_SD_PATH));
		}

		return paths;
	}

	/**
	 * Return a file with a absolute path to the application-specific directory on the external sd card. The format is
	 * <code>/sdCardPath/Android/data/application package name/files</code>
	 * 
	 * @param context
	 * @param sdCardPath
	 * @return
	 */
	private static File getSdCardFilesDir(Context context, String sdCardPath)
	{
		// Assemble the dir path
		File sdCardFile = new File(sdCardPath + File.separator
				+ "Android/data" + File.separator 
				+ context.getApplicationContext().getPackageName() + File.separator
				+ "files" + File.separator);

		// Create the path
		sdCardFile.mkdirs();

		return sdCardFile;
	}

	/**
	 * Compare if two strings are equal and also perform <code>null</code> and <code>isEmpty()</code> checks
	 * 
	 * @param firstString
	 * @param secondString
	 * @return
	 */
	private static boolean compare(String firstString, String secondString)
	{
		return firstString != null && secondString != null && !firstString.isEmpty() && !secondString.isEmpty() && firstString.equalsIgnoreCase(secondString);
	}

	/**
	 * Add an element to the end of a File array
	 * 
	 * @param paths
	 * @param path
	 * @return
	 */
	public static File[] addPath(File[] paths, File path)
	{
		// Create a new paths array
		File[] newPaths = new File[paths.length + 1];
		// Copy the old paths and add the new one at the end of the array
		System.arraycopy(paths, 0, newPaths, 0, paths.length);
		newPaths[paths.length] = path;

		return newPaths;
	}

}
