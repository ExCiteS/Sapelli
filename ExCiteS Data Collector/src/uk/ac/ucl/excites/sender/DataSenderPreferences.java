package uk.ac.ucl.excites.sender;

import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.sender.util.Constants;
import uk.ac.ucl.excites.sender.util.ServiceChecker;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.util.Log;

/**
 * This class contains various utilities methods
 * 
 * @author Michalis Vitos
 * 
 */
public class DataSenderPreferences extends PreferenceActivity implements OnSharedPreferenceChangeListener
{

	public static final String PREFERENCES = "ExCiteS_Data_Sender_Preferences";
	public static final String TAG = "DataSenderPreferences";

	@SuppressWarnings("deprecation")
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		addPreferencesFromResource(R.xml.background_preferences);
		PreferenceManager.setDefaultValues(DataSenderPreferences.this, R.xml.background_preferences, false);

		// Register a listener
		SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		sharedPreferences.registerOnSharedPreferenceChangeListener(this);

		// Debug the Preferences
		if(Constants.DEBUG_LOG)
			printPreferences(getApplicationContext());

		// Start Button
		Preference startButton = findPreference("startButton");
		startButton.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener()
		{
			@Override
			public boolean onPreferenceClick(Preference arg0)
			{
				// Call the Service
				Intent intent = new Intent(DataSenderPreferences.this, DataSenderService.class);
				startService(intent);
				return true;
			}
		});
	}

	/**
	 * Check if the data sender should run
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean getSenderEnabled(Context mContext)
	{
		SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return sharedPreferences.getBoolean("enableSender", false);
	}
	
	/**
	 * Check if the phone should get in Airplane Mode
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean getAirplaneMode(Context mContext)
	{
		SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return sharedPreferences.getBoolean("airplaneMode", false);
	}

//	/**
//	 * Get the Phone Number of the centre phone that works as a relay
//	 * 
//	 * @param mContext
//	 * @return
//	 */
//	public static String getCenterPhoneNumber(Context mContext)
//	{
//		SharedPreferences mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
//		return mSharedPreferences.getString("centerPhoneNumber", "");
//	}

	/**
	 * Check if the phone should upload to Dropbox
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean getDropboxUpload(Context mContext)
	{
		SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return sharedPreferences.getBoolean("dropboxUpload", false);
	}

	/**
	 * Check if the phone should upload via SMS
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean getSMSUpload(Context mContext)
	{
		SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return sharedPreferences.getBoolean("smsUpload", true);
	}
	
	/**
	 * Get the number of minutes that the service is checking for connectivity
	 * 
	 * @param mContext
	 * @return
	 */
	public static int getTimeSchedule(Context mContext)
	{
		SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return Integer.parseInt(sharedPreferences.getString("timeSchedule", "1"));
	}

	/**
	 * Get the number of seconds that the service will wait for connectivity.
	 * 
	 * @param mContext
	 * @return
	 */
	public static int getMaxAttempts(Context mContext)
	{
		SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(mContext);
		return Integer.parseInt(sharedPreferences.getString("maxAttempts", "1"));
	}

	@Override
	public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key)
	{

		if(Constants.DEBUG_LOG)
		{
			Log.i(TAG, "onSharedPreferenceChanged(): key = " + key);
			printPreferences(getApplicationContext());
		}

		// Restart the Service
		ServiceChecker.restartActiveDataSender(this);
	}

	public static void printPreferences(Context context)
	{
		Log.d(TAG, "------------ Preferences: -------------");
		Log.d(TAG, "Sender enabled: " + (getSenderEnabled(context) ? "true" : "false"));
		Log.d(TAG, "AirplaneMode: " + (getAirplaneMode(context) ? "true" : "false"));
		//Log.d(TAG, "CenterPhoneNumber: " + getCenterPhoneNumber(mContext));
		Log.d(TAG, "TimeSchedule: " + getTimeSchedule(context));
		Log.d(TAG, "MaxAttempts: " + getMaxAttempts(context));
		Log.d(TAG, "SMSUpload: " + (getSMSUpload(context) ? "true" : "false"));
		Log.d(TAG, "DropboxUpload: " + (getDropboxUpload(context) ? "true" : "false"));
	}
}
