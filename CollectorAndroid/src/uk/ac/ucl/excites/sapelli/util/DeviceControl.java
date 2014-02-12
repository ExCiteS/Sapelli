package uk.ac.ucl.excites.sapelli.util;

import java.io.File;

import android.annotation.TargetApi;
import android.content.Context;
import android.content.Intent;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
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
	
	private DeviceControl() //class should not be instantiated
	{}
	
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
	 * Toggle thought the AirplaneMode
	 */
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
	@SuppressWarnings("deprecation")
	public static void toggleAirplaneMode(Context context)
	{
		boolean isInAirplaneMode = inAirplaneMode(context);
		try
		{
			// If airplane mode is on, value 0, else value is 1
			if(Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1)
				Settings.System.putInt(context.getContentResolver(), Settings.System.AIRPLANE_MODE_ON, isInAirplaneMode ? 0 : 1);
			else
				Settings.Global.putInt(context.getContentResolver(), Settings.Global.AIRPLANE_MODE_ON, isInAirplaneMode ? 0 : 1);

			// Reload when the mode is changed each time by sending Intent
			Intent intent = new Intent(Intent.ACTION_AIRPLANE_MODE_CHANGED);
			intent.putExtra("state", !isInAirplaneMode);
			context.sendBroadcast(intent);

			Debug.d("Airplane mode is: " + (isInAirplaneMode ? "OFF" : "ON"));
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
			{	// Play the sound
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
	
}
