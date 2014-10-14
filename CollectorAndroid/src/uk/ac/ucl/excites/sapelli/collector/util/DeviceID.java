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

package uk.ac.ucl.excites.sapelli.collector.util;

import java.math.BigInteger;
import java.util.zip.CRC32;

import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.util.Debug;
import uk.ac.ucl.excites.sapelli.util.DeviceControl;
import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.bluetooth.BluetoothAdapter;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.net.wifi.WifiManager;
import android.os.Build;
import android.os.SystemClock;
import android.provider.Settings.Secure;
import android.telephony.TelephonyManager;

/**
 * Class that provides various ways to uniquely (or as unique as possible) identify an Android device<br/>
 * 
 * Based on: <a href="http://www.pocketmagic.net/2011/02/android-unique-device-id/#.USeA7KXvh8H">http://www.pocketmagic.net/2011/02/android-unique-device-id/#.USeA7KXvh8H</a>
 * 
 * @author mstevens, Michalis Vitos
 */
public class DeviceID
{

	// Statics ------------------------------------------------------

	// Preferences
	private static final String PREFERENCES = "DEVICEID_PREFERENCES";
	private static final String PREF_DEVICE_ID = "DEVICEID_ID";
	private static final String PREF_IMEI = "DEVICEID_IMEI";
	private static final String PREF_WIFI_MAC = "DEVICEID_WIFI_MAC";
	private static final String PREF_BLUETOOTH_MAC = "DEVICEID_BLUETOOTH_MAC";
	private static final String PREF_ANDROID_ID = "DEVICEID_ANDROID_ID";
	private static final String PREF_HARWARE_SERIAL = "DEVICEID_HARWARE_SERIAL";
	private static final String[] DEVICE_ID_SOURCE_PREF_ORDER = { PREF_IMEI, PREF_WIFI_MAC, PREF_BLUETOOTH_MAC, PREF_ANDROID_ID, PREF_HARWARE_SERIAL };

	private static DeviceID INSTANCE = null;
	
	static public void Initialise(Context context, InitialisationCallback client)
	{
		new Initialiser(context, client); // may start async task!
	}
	
	static public DeviceID GetInstanceOrNull()
	{
		return INSTANCE;
	}
	
	static public DeviceID GetInstance(Context context) throws IllegalStateException
	{
		if(INSTANCE == null)
		{
			DeviceID id = new DeviceID(context);
			if(id.isInitialised())
				INSTANCE = id;
		}
		if(INSTANCE != null)
			return INSTANCE;
		else
			throw new IllegalStateException("DeviceID was not initialised. Please call Initialise() first.");
	}
	
	/**
	 * Returns a string with device hardware info.
	 * This method is static and therefore does not require an initialised DeviceID object 
	 * 
	 * @param separator
	 * @return
	 */
	static public String getHardwareInfo(String separator)
	{
		StringBuilder bldr = new StringBuilder();
		bldr.append("Brand: " + Build.BRAND + separator);
		bldr.append("Model: " + Build.MODEL + separator);
		bldr.append("Product: " + Build.PRODUCT + separator);
		bldr.append("Device: " + Build.DEVICE + separator);
		bldr.append("Hardware: " + Build.HARDWARE + separator);
		bldr.append("Board: " + Build.BOARD + separator);
		bldr.append("Display: " + Build.DISPLAY + separator);
		bldr.append("ID: " + Build.ID + separator);
		bldr.append("CPU_ABI: " + Build.CPU_ABI + separator);
		bldr.append("CPU_ABI2: " + Build.CPU_ABI2);
		return bldr.toString();
	}
	
	/**
	 * Returns a string with device software info.
	 * This method is static and therefore does not require an initialised DeviceID object 
	 * 
	 * @param separator
	 * @return
	 */
	static public String getSoftwareInfo(String separator)
	{
		StringBuilder bldr = new StringBuilder();
		bldr.append("Release: " + Build.VERSION.RELEASE + separator);
		bldr.append("API level: " + Build.VERSION.SDK_INT + separator);
		bldr.append("Codename/REL: " + Build.VERSION.CODENAME + separator);
		bldr.append("Incremental: " + Build.VERSION.INCREMENTAL + separator);
		bldr.append("Bootloader: " + Build.BOOTLOADER + separator);
		bldr.append("Build fingerprint: " + Build.FINGERPRINT);
		return bldr.toString();
	}
	
	// Dynamics -----------------------------------------------------
	private SharedPreferences preferences;
	
	private DeviceID(Context context)
	{
		this.preferences = context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);
		
		// Debug
		//printInfo();
	}
	
	private void saveStingPreference(String key, String value)
	{
		SharedPreferences.Editor editor = preferences.edit();
		editor.putString(key, value);
		editor.commit();
	}

	/**
	 * Retrieve IMEI from the preferences
	 * 
	 * @return IMEI or null
	 */
	public String getIMEI()
	{
		return preferences.getString(PREF_IMEI, null);
	}

	/**
	 * Retrieve the WiFi address from the preferences
	 * 
	 * @return the address or null
	 */
	public String getWiFiMACAddress()
	{
		return preferences.getString(PREF_WIFI_MAC, null);
	}

	/**
	 * Retrieve the Bluetooth Address from the preferences
	 * 
	 * @return the address or null
	 */
	public String getBluetoothMACAddress()
	{
		return preferences.getString(PREF_BLUETOOTH_MAC, null);
	}

	/**
	 * Retrieve the Android ID
	 * 
	 * @return AndroidID or null
	 */
	public String getAndroidID()
	{
		return preferences.getString(PREF_ANDROID_ID, null);
	}

	/**
	 * Retrieve the Hardware Serial Number
	 * 
	 * @return serial number or null
	 */
	public String getHardwareSerialNumber()
	{
		return preferences.getString(PREF_HARWARE_SERIAL, null);
	}

	/**
	 * Retrieve Collected Info from the preferences or null
	 * 
	 * @return
	 */
	public String getRawID()
	{
		return preferences.getString(PREF_DEVICE_ID, null);
	}
	
	public boolean isInitialised()
	{
		return getRawID() != null;
	}

	/**
	 * Returns a 32 bit String hash code based on assembled device information
	 * 
	 * @return the hash code
	 * @see java.lang.String#hashCode()
	 */
	public int getIDAsStringHash()
	{
		return getRawID().hashCode();
	}

	/**
	 * Returns a 32 bit unsigned CRC hash code based on assembled device information
	 * 
	 * @return the hash code
	 * @see <a href="http://en.wikipedia.org/wiki/Cyclic_redundancy_check">http://en.wikipedia.org/wiki/Cyclic_redundancy_check</a>
	 */
	public long getIDAsCRC32Hash()
	{
		CRC32 crc32 = new CRC32();
		crc32.update(getRawID().getBytes());
		return crc32.getValue();
	}

	/**
	 * Returns a 128 bit MD5 hash code based on assembled device information
	 * 
	 * @return the hash code
	 * @see <a href="http://en.wikipedia.org/wiki/MD5">http://en.wikipedia.org/wiki/MD5</a>
	 */
	public BigInteger getIDAsMD5Hash()
	{
		return Hashing.getMD5Hash(getRawID().getBytes());
	}

	/**
	 * Print the Preferences for debugging
	 */
	@SuppressLint("DefaultLocale")
	public void printInfo()
	{
		Debug.d("------------------------------------");
		Debug.d("IMEI: " + getIMEI());
		Debug.d("WiFi MAC address: " + getWiFiMACAddress());
		Debug.d("Bluetooth MAC address: " + getBluetoothMACAddress());
		Debug.d("Android ID: " + getAndroidID());
		Debug.d("Hardware serial number: " + getHardwareSerialNumber());
		Debug.d("Device ID: " + getRawID());
		Debug.d("String hash of device ID: " + getIDAsStringHash());
		Debug.d("CRC32 hash of device ID: " + getIDAsCRC32Hash());
		Debug.d("MD5 hash of device ID: " + getIDAsMD5Hash());
		Debug.d("------------------------------------");
	}
	
	public interface InitialisationCallback
	{

		public void initialisationSuccess(DeviceID deviceID);
		
		public void initialisationFailure(DeviceID deviceID); 
		
	}
	
	private static class Initialiser extends AsyncTaskWithWaitingDialog<Void, Void, Integer>
	{

		// Statics --------------------------------------------------
		private static final int WAITING_TIME_PER_STEP = 250; // ms
		private static final int MAX_STEPS = 40; // Max waiting time: 40*250 = 10 sec

		// AsyncTask Result Codes
		private static final int RESULT_OK = 0;
		private static final int RESULT_AIRPLANE_MODE = -1;
				
		// Dynamics -------------------------------------------------
		private DeviceID id;
		private InitialisationCallback caller;
		private Context context;
		private WifiManager wifiManager;
		private BluetoothAdapter bluetoothAdapter;
		private BroadcastReceiver wiFiBroadcastReceiver;
		private BroadcastReceiver bluetoothBroadcastReceiver;
		private boolean inAirplaneMode = false;
		
		public Initialiser(Context context, InitialisationCallback caller)
		{
			super(context, "Determining device ID..."); //TODO multilang
			this.context = context;
			this.caller = caller;
			this.id = new DeviceID(context);

			// Check if the we already have a raw device ID, if not run the AsyncTask:
			if(!this.id.isInitialised())
			{
				wifiManager = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
				bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
				execute(); // async!
			}
			else
				caller.initialisationSuccess(id);
		}

		@Override
		protected Integer doInBackground(Void... params)
		{
			// Check if in flight mode, otherwise IMEI and Bluetooth do not work on some devices i.e. Samsung Xcover
			if(DeviceControl.inAirplaneMode(context))
			{
				inAirplaneMode = true;
				if(DeviceControl.canToogleAirplaneMode())
				{
					DeviceControl.disableAirplaneMode(context);
					int counter = 0;
					while(DeviceControl.inAirplaneMode(context) && counter < MAX_STEPS)
					{
						counter++;
						SystemClock.sleep(WAITING_TIME_PER_STEP); // Wait for the phone to get out of AirplaneMode
					}
				}
				else
					return RESULT_AIRPLANE_MODE;
			}

			//Debug.d("The Device ID is being initialised.");

			// Call the functions to initialise IMEI, Android ID, Hardware Serial Number, Wi-Fi MAC and Bluetooth MAC:
			initIMEI();
			initAndroidID();
			initHardwareSerialNumber();
			initWiFiAddress(); // may set up a broadcast receiver
			initBluetoothAddress(); // may set up a broadcast receiver
			
			// Wait until the app finds the Wifi and Bluetooth addresses of the device
			int counter = 0;
			while(counter < MAX_STEPS)
			{
				counter++;
				if(((wifiManager != null) && (id.getWiFiMACAddress() == null)) || ((bluetoothAdapter != null) && (id.getBluetoothMACAddress() == null)))
					SystemClock.sleep(WAITING_TIME_PER_STEP); // Wait for the initialisation
				else
					break;
			}
			return RESULT_OK;
		}

		@Override
		protected void onPostExecute(Integer result)
		{
			super.onPostExecute(result); // to dismiss waiting dialog
			switch(result)
			{
				case RESULT_OK:
				{	
					// Put the phone back in AirplaneMode (because it was in airplane mode before):
					if(inAirplaneMode)
						DeviceControl.enableAirplaneMode(context);
					
					// Compute and save the Device ID
					computeDeviceID();
					
					// Debug
					//id.printInfo();
					
					// Callback caller:
					if(id.isInitialised())
					{
						INSTANCE = id;
						caller.initialisationSuccess(id);
					}
					else
						caller.initialisationFailure(id);
					break;
				}
				case RESULT_AIRPLANE_MODE:
					showAirplaneDialog(context);
					break;
			}
		}
		
		/**
		 * A method to compute and store a valid device ID
		 */
		private void computeDeviceID()
		{
			String rawID = null;
			for(String pref : DEVICE_ID_SOURCE_PREF_ORDER)
			{
				rawID = id.preferences.getString(pref, null);
				if(rawID != null)
					break;
			}
			if(rawID != null)
				id.saveStingPreference(PREF_DEVICE_ID, rawID); // Save the raw device ID to preferences			
		}
		
		/**
		 * Find and save the IMEI to the preferences
		 * 
		 * @param imei
		 */
		private void initIMEI()
		{
			TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
			if(tm != null)
				id.saveStingPreference(PREF_IMEI, tm.getDeviceId()); // Save IMEI to preferences
		}
		
		/**
		 * Find and save the MAC address of the Wi-Fi NIC. The method will attempt to enable the WiFi, get the MAC address and disable again the adapter.
		 * 
		 * Requires android.permission.ACCESS_WIFI_STATE
		 */
		private void initWiFiAddress()
		{
			if(wifiManager != null && wifiManager.isWifiEnabled())
				id.saveStingPreference(PREF_WIFI_MAC, wifiManager.getConnectionInfo().getMacAddress()); // Save WiFi MAC address to preferences
			else if(wifiManager != null)
			{	// Try to enable the WiFi Adapter and wait for the broadcast receiver:
				setWifiBroadcastReceiver();
				wifiManager.setWifiEnabled(true);
			}
		}
		
		/**
		 * Broadcast receiver to handle WiFi states
		 */
		private void setWifiBroadcastReceiver()
		{
			wiFiBroadcastReceiver = new BroadcastReceiver()
			{
				@Override
				public void onReceive(Context context, Intent intent)
				{
					int extraWifiState = intent.getIntExtra(WifiManager.EXTRA_WIFI_STATE, WifiManager.WIFI_STATE_UNKNOWN);
					switch(extraWifiState)
					{
						case WifiManager.WIFI_STATE_DISABLED:
							Debug.d("WiFi Disabled");
							break;
						case WifiManager.WIFI_STATE_DISABLING:
							Debug.d("WiFi Disabling");
							break;
						case WifiManager.WIFI_STATE_ENABLED:
							Debug.d("WiFi Enabled");
							// Save WiFi MAC address to preferences:
							id.saveStingPreference(PREF_WIFI_MAC, wifiManager.getConnectionInfo().getMacAddress());
							// Try to disable WiFi:
							wifiManager.setWifiEnabled(false);
							// Unregister the Broadcast Receiver:
							context.unregisterReceiver(this);
							break;
						case WifiManager.WIFI_STATE_ENABLING:
							Debug.d("WiFi Enabling");
							break;
						case WifiManager.WIFI_STATE_UNKNOWN:
							Debug.d("WiFi Unknown");
							break;
					}
				}
			};
		
			// Register for broadcasts on WiFi Manager state change
			IntentFilter wifiFilter = new IntentFilter(WifiManager.WIFI_STATE_CHANGED_ACTION);
			context.registerReceiver(wiFiBroadcastReceiver, wifiFilter);
		}
		
		/**
		 * Find and save the MAC address of the Bluetooth NIC. The method will attempt to enable the Bluetooth, get the address and disable again the adapter.
		 * 
		 * Requires android.permission.BLUETOOTH
		 * 
		 */
		private void initBluetoothAddress()
		{
			if(bluetoothAdapter != null && bluetoothAdapter.isEnabled())
				id.saveStingPreference(PREF_BLUETOOTH_MAC, bluetoothAdapter.getAddress()); // Save Bluetooth MAC address to preferences
			else if(wifiManager != null)
			{	// Try to enable the Bluetooth Adapter and wait for the broadcast receiver:
				setBluetoothBroadcastReceiver();
				bluetoothAdapter.enable();
			}
		}

		/**
		 * Broadcast receiver to handle Bluetooth states
		 */
		private void setBluetoothBroadcastReceiver()
		{
			bluetoothBroadcastReceiver = new BroadcastReceiver()
			{
				@Override
				public void onReceive(Context context, Intent intent)
				{
					final String action = intent.getAction();
					if(action.equals(BluetoothAdapter.ACTION_STATE_CHANGED))
					{
						final int state = intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, BluetoothAdapter.ERROR);
						switch(state)
						{
							case BluetoothAdapter.STATE_OFF:
								Debug.d("Bluetooth off");
								break;
							case BluetoothAdapter.STATE_TURNING_OFF:
								Debug.d("Turning Bluetooth off...");
								break;
							case BluetoothAdapter.STATE_ON:
								//Debug.d("Bluetooth on");
								// Save Bluetooth MAC address to preferences:
								id.saveStingPreference(PREF_BLUETOOTH_MAC, bluetoothAdapter.getAddress());
								// Try to disable Bluetooth
								bluetoothAdapter.disable();
								// Unregister the Broadcast Receiver
								context.unregisterReceiver(this);
								break;
							case BluetoothAdapter.STATE_TURNING_ON:
								Debug.d("Turning Bluetooth on...");
								break;
						}
					}
				}
			};
		
			// Register for broadcasts on BluetoothAdapter state change
			IntentFilter bluetoothFilter = new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED);
			context.registerReceiver(bluetoothBroadcastReceiver, bluetoothFilter);
		}
		
		/**
		 * Find and save the Android ID to the preferences
		 */
		private void initAndroidID()
		{
			id.saveStingPreference(PREF_ANDROID_ID, Secure.getString(context.getContentResolver(), Secure.ANDROID_ID));
		}
		
		/**
		 * Find and save the hardware serial number to the preferences
		 */
		private void initHardwareSerialNumber()
		{
			final String serial = Build.SERIAL;
			if("unknown".equalsIgnoreCase(serial))
				id.saveStingPreference(PREF_HARWARE_SERIAL, null);
			else
				id.saveStingPreference(PREF_HARWARE_SERIAL, serial);
		}
		
		/**
		 * @param context
		 */
		private void showAirplaneDialog(final Context context)
		{
			AlertDialog.Builder builder = new AlertDialog.Builder(context); // TODO multilang
			builder.setMessage("In order for the Sapelli to be initialised for first use, please take your device out of Airplane Mode.").setCancelable(false)
					.setPositiveButton("OK", new DialogInterface.OnClickListener()
			{
				public void onClick(DialogInterface dialog, int id)
				{
					// Close the app by going to HOME
					Intent intent = new Intent(Intent.ACTION_MAIN);
					intent.addCategory(Intent.CATEGORY_HOME);
					intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					context.startActivity(intent);
				}
			});
			AlertDialog alert = builder.create();
			alert.show();
		}
		
	}
	
}