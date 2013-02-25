package uk.ac.excites.sender;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;

/**
 * This class contains various utilities methods
 * 
 * @author Michalis Vitos
 * 
 */
public class SenderBackgroundPreferences extends PreferenceActivity
{

	public static final String PREFERENCES = "ExCiteS_Data_Sender_Preferences";

	@SuppressWarnings("deprecation")
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		addPreferencesFromResource(R.xml.background_preferences);
		PreferenceManager.setDefaultValues(SenderBackgroundPreferences.this, R.xml.background_preferences, false);
		
		// Call the Service
		Intent mIntent = new Intent(this, SenderBackgroundService.class);
		startService(mIntent);
	}
	
	/**
	 * Check if the phone should upload to Dropbox
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean getDropboxUpload(Context mContext)
	{
		SharedPreferences mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return mSharedPreferences.getBoolean("dropboxUpload", true);
	}

	
	/**
	 * Check if the phone should get in Airplane Mode
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean getAirplaneMode(Context mContext)
	{
		SharedPreferences mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return mSharedPreferences.getBoolean("airplaneMode", true);
	}
	
	/**
	 * Get the Phone Number of the centre phone that works as a rely
	 * 
	 * @param mContext
	 * @return
	 */
	public static String getCenterPhoneNumber(Context mContext)
	{
		SharedPreferences mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return mSharedPreferences.getString("centerPhoneNumber", "");
	}

	/**
	 * Get the number of minutes that the service is checking for connectivity
	 * 
	 * @param mContext
	 * @return
	 */
	public static int getTimeSchedule(Context mContext)
	{
		SharedPreferences mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return Integer.parseInt(mSharedPreferences.getString("timeSchedule", "1"));
	}

	/**
	 * Get the number of seconds that the service will wait for connectivity.
	 * 
	 * @param mContext
	 * @return
	 */
	public static int getMaxAttemps(Context mContext)
	{
		SharedPreferences mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return Integer.parseInt(mSharedPreferences.getString("maxAttemps", "1"));
	}
}
