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

package uk.ac.ucl.excites.sapelli.transmission.sender;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.sender.util.Constants;
import uk.ac.ucl.excites.sapelli.sender.util.ServiceChecker;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.util.Log;

/**
 * This class contains various utilities methods
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
public class DataSenderPreferences extends PreferenceActivity implements OnSharedPreferenceChangeListener
{

	private static final String TAG = "DataSenderPreferences";
	
	public static final String PREFERENCES = "ExCiteS_Data_Sender_Preferences";
	private static final String PREF_AIRPLANE_MODE = "airplaneMode";
	private static final String PREF_DROPBOX_UPLOAD = "dropboxUpload";
	private static final String PREF_SMS_UPLOAD = "smsUpload";
	private static final String PREF_MAX_ATTEMPTS = "maxAttempts";
	private static final String PREF_TIME_SCHEDULE = "timeSchedule";
	private static final String PREF_ENABLE_SENDER = "enableSender";

	@SuppressWarnings("deprecation")
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		addPreferencesFromResource(R.xml.background_preferences); //TODO remove use of deprecated method
		PreferenceManager.setDefaultValues(DataSenderPreferences.this, R.xml.background_preferences, false);

		// Register a listener
		SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		sharedPreferences.registerOnSharedPreferenceChangeListener(this);

		// Debug the Preferences
		if(Constants.DEBUG_LOG)
			printPreferences(getApplicationContext());
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
		return sharedPreferences.getBoolean(PREF_ENABLE_SENDER, false);
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
		return sharedPreferences.getBoolean(PREF_AIRPLANE_MODE, false);
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
		return sharedPreferences.getBoolean(PREF_DROPBOX_UPLOAD, false);
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
		return sharedPreferences.getBoolean(PREF_SMS_UPLOAD, true);
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
		return ((sharedPreferences.getString(PREF_TIME_SCHEDULE, "1")).equals("")) ? 1 : Integer.parseInt(sharedPreferences.getString(PREF_TIME_SCHEDULE, "10"));
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
		return Integer.parseInt(sharedPreferences.getString(PREF_MAX_ATTEMPTS, "1"));
	}

	@Override
	public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key)
	{
		Context context = getApplicationContext();

		if(Constants.DEBUG_LOG)
		{
			Debug.d("key: " + key);
			printPreferences(context);
		}
		
		// ================================================================================
		// ================================================================================

		if(key.equals(PREF_ENABLE_SENDER))
		{
			// If true start service, otherwise try to stop it
			if(getSenderEnabled(context))
			{
				ServiceChecker.startService(context);
			}
			else
			{
				ServiceChecker.stopService(context);
				Intent intent = new Intent(context, DataSenderPreferences.class);
				intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
				context.startActivity(intent);
			}
		}
		else if(key.equals(PREF_TIME_SCHEDULE))
		{
			if(getTimeSchedule(context) <= 0)
			{
				// Show a message
				final AlertDialog.Builder builder = new AlertDialog.Builder(this);
				builder.setTitle("Invalid Input");
				builder.setMessage("Please insert an interval value higher than 0.");
				builder.setPositiveButton(android.R.string.ok, null);
				builder.show();
			}
			else
			{
				// Restart the Service
				ServiceChecker.restartActiveDataSender(context);
			}
		}
		else
		{
			// Restart the Service
			ServiceChecker.restartActiveDataSender(context);
		}
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
