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

package uk.ac.ucl.excites.sapelli.util;

import java.io.File;

import android.annotation.TargetApi;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.os.BatteryManager;
import android.os.Build;
import android.os.Vibrator;
import android.provider.Settings;
import android.util.Log;

/**
 * @author Michalis Vitos, mstevens
 * 
 */
public final class DeviceControl
{

	private static final String TAG = "DeviceControl";
	// Time to wait after exiting the Airplane Mode for GSM to be connected
	public static final int POST_AIRPLANE_MODE_WAITING_TIME = 30;

	private DeviceControl() // class should not be instantiated
	{
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
	 * @param enabled
	 *            (True to set the device in Airplane Mode)
	 */
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
	@SuppressWarnings("deprecation")
	private static void setAirplaneMode(Context context, boolean enabled)
	{
		try
		{
			// If airplane mode is on, value 0, else value is 1
			if(canToogleAirplaneMode())
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

	public static boolean canToogleAirplaneMode()
	{
		return (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1) ? true : false;
	}

	public static void vibrate(Context context, int durationMS)
	{
		Vibrator vibrator = (Vibrator) context.getSystemService(Context.VIBRATOR_SERVICE);
		vibrator.vibrate(durationMS);
	}

	public static void playSoundFile(Context context, File soundFile)
	{
		try
		{
			if(soundFile.exists()) // check if the file really exists
			{ // Play the sound
				MediaPlayer mp = MediaPlayer.create(context, Uri.fromFile(soundFile));
				mp.start();
				mp.setOnCompletionListener(new OnCompletionListener()
				{
					@Override
					public void onCompletion(MediaPlayer mp)
					{
						mp.release();
					}
				});
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon playing sound file.", e);
		}
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
}
