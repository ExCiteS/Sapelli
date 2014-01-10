package uk.ac.ucl.excites.collector.util;

import java.math.BigInteger;
import java.util.zip.CRC32;

import uk.ac.ucl.excites.transmission.crypto.Hashing;
import uk.ac.ucl.excites.util.Debug;
import android.app.ProgressDialog;
import android.bluetooth.BluetoothAdapter;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.net.wifi.WifiManager;
import android.os.AsyncTask;
import android.os.Build;
import android.os.SystemClock;
import android.provider.Settings.Secure;
import android.telephony.TelephonyManager;

/**
 * Class that provides various ways to uniquely (or as unique as possible) identify an Android device<br/>
 * 
 * Based on: <a
 * href="http://www.pocketmagic.net/2011/02/android-unique-device-id/#.USeA7KXvh8H">http://www.pocketmagic.net/2011/02/android-unique-device-id/#.USeA7KXvh8H
 * </a>
 * 
 * @author mstevens, Michalis Vitos
 */
public class DeviceID extends AsyncTask<Void, Void, Void>
{

	// Statics
	private static final long PRIME = 1125899906842597L;
	private static final char SEPARATOR = '|';

	private static final int WAITING_TIME_PER_STEP = 250; // ms
	private static final int MAX_STEPS = 40; // Max waiting time: 40*250 = 10 sec

	// Preferences
	public static final String PREFERENCES = "DEVICEID_PREFERENCES";
	private static final String PREF_DEVICE_ID = "DEVICEID_ID";
	private static final String PREF_IMEI = "DEVICEID_IMEI";
	private static final String PREF_WIFI_MAC = "DEVICEID_WIFI_MAC";
	private static final String PREF_BLUETOOTH_MAC = "DEVICEID_BLUETOOTH_MAC";
	private static final String PREF_ANDROID_ID = "DEVICEID_ANDROID_ID";
	private static final String PREF_HARWARE_SERIAL = "DEVICEID_HARWARE_SERIAL";

	// Dynamics
	private Context context;
	private ProgressDialog progress;
	private WifiManager wifiManager;
	private BluetoothAdapter bluetoothAdapter;
	private SharedPreferences preferences;
	private BroadcastReceiver wiFiBroadcastReceiver;
	private BroadcastReceiver bluetoothBroadcastReceiver;

	public DeviceID(Context context)
	{
		this.context = context;
		preferences = context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);

		// Debug
		printPreferences();

		// Check if the app has a device ID, if not run the AsyncTask
		if(getDeviceID() == null)
		{
			wifiManager = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
			bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
			execute();
		}
	}

	@Override
	protected void onPreExecute()
	{
		progress = new ProgressDialog(context);
		progress.setMessage("Loading");
		progress.setCancelable(false);
		progress.show();
	}

	@Override
	protected Void doInBackground(Void... params)
	{
		initialise();

		int counter = 0;

		// Wait until the app finds the Wifi and Bluetooth addresses of the device
		while(counter < MAX_STEPS)
		{
			counter++;

			if(((wifiManager != null) && (retrieveWiFiMACAddress() == null)) || ((bluetoothAdapter != null) && (retrieveBluetoothMACAddress() == null)))
				SystemClock.sleep(WAITING_TIME_PER_STEP); // Wait for the initialisation
			else
				break;
		}

		return null;
	}

	@Override
	protected void onPostExecute(Void result)
	{
		// Compute and save the Device ID
		computeDeviceID();

		// Debug
		printPreferences();

		progress.dismiss();
	}

	/**
	 * Initialise the various elements that are used as a device ID
	 */
	private void initialise()
	{
		Debug.d("The Device ID is being initialised.");

		// Call the functions to initialise IMEI, Wi-Fi MAC
		// Bluetooth MAC, Android ID and Hardware Serial Number
		saveIMEI();
		saveWiFiAddress();
		saveBluetoothAddress();
		saveAndroidID();
		saveHardwareSerialNumber();
	}

	public String getDeviceInfo()
	{
		return Build.BRAND + SEPARATOR + Build.MANUFACTURER + SEPARATOR + Build.MODEL + SEPARATOR + Build.PRODUCT + SEPARATOR + Build.DEVICE + SEPARATOR
				+ Build.DISPLAY + SEPARATOR + Build.ID + SEPARATOR + Build.BOARD + SEPARATOR + Build.HARDWARE + SEPARATOR + Build.CPU_ABI + SEPARATOR
				+ Build.CPU_ABI2 + SEPARATOR + Build.VERSION.CODENAME + SEPARATOR + Build.VERSION.INCREMENTAL + SEPARATOR + Build.VERSION.SDK_INT + SEPARATOR
				+ Build.BOOTLOADER + SEPARATOR + Build.FINGERPRINT + SEPARATOR;
	}

	/**
	 * Find and save the IMEI to the preferences
	 * 
	 * @param imei
	 */
	private void saveIMEI()
	{
		TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);

		if(tm != null)
		{
			SharedPreferences.Editor editor = preferences.edit();
			editor.putString(PREF_IMEI, tm.getDeviceId());
			editor.commit();
		}
	}

	/**
	 * Retrieve IMEI from the preferences
	 * 
	 * @return IMEI or null
	 */
	public String retrieveIMEI()
	{
		return preferences.getString(PREF_IMEI, null);
	}

	/**
	 * Find and save the MAC address of the Wi-Fi NIC. The method will attempt to enable the WiFi, get the MAC address and disable again the adapter.
	 * 
	 * Requires android.permission.ACCESS_WIFI_STATE
	 */
	private void saveWiFiAddress()
	{
		if(wifiManager != null && wifiManager.isWifiEnabled())
		{
			String wifiAddress = wifiManager.getConnectionInfo().getMacAddress();
			// Save WiFi Address to preferences
			saveWiFiAddress(wifiAddress);
		}
		// Try to enable the WiFi Adapter and wait for the broadcast receiver
		else if(wifiManager != null)
		{
			setWifiBroadcastReceiver();
			wifiManager.setWifiEnabled(true);
		}
	}


	/**
	 * Save the WiFi MAC Address to the preferences
	 * 
	 * @param wifiAddress
	 */
	private void saveWiFiAddress(String wifiAddress)
	{
		SharedPreferences.Editor editor = preferences.edit();
		editor.putString(PREF_WIFI_MAC, wifiAddress);
		editor.commit();
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
	
					saveWiFiAddress(wifiManager.getConnectionInfo().getMacAddress());
	
					// Try to disable WiFi
					wifiManager.setWifiEnabled(false);
	
					// Unregister the Broadcast Receiver
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
	 * Retrieve the WiFi address from the preferences
	 * 
	 * @return the address or null
	 */
	public String retrieveWiFiMACAddress()
	{
		return preferences.getString(PREF_WIFI_MAC, null);
	}

	/**
	 * Find and save the MAC address of the Bluetooth NIC. The method will attempt to enable the Bluetooth, get the address and disable again the adapter.
	 * 
	 * Requires android.permission.BLUETOOTH
	 * 
	 */
	private void saveBluetoothAddress()
	{
		if(bluetoothAdapter != null && bluetoothAdapter.isEnabled())
		{
			String bluetoothAddress = bluetoothAdapter.getAddress();
			// Save Bluetooth Address to preferences
			saveBluetoothAddress(bluetoothAddress);
		}
		// Try to enable the Bluetooth Adapter and wait for the broadcast receiver
		else if(wifiManager != null)
		{
			setBluetoothBroadcastReceiver();
			bluetoothAdapter.enable();
		}
	}

	/**
	 * Save the Bluetooth MAC Address to the preferences
	 * 
	 * @param bluetoothAddress
	 */
	private void saveBluetoothAddress(String bluetoothAddress)
	{
		SharedPreferences.Editor editor = preferences.edit();
		editor.putString(PREF_BLUETOOTH_MAC, bluetoothAddress);
		editor.commit();
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
	
						Debug.d("Bluetooth on");
	
						saveBluetoothAddress(bluetoothAdapter.getAddress());
	
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
	 * Retrieve the Bluetooth Address from the preferences
	 * 
	 * @return the address or null
	 */
	public String retrieveBluetoothMACAddress()
	{
		return preferences.getString(PREF_BLUETOOTH_MAC, null);
	}

	/**
	 * Find and save the Android ID to the preferences
	 */
	private void saveAndroidID()
	{
		SharedPreferences.Editor editor = preferences.edit();
		editor.putString(PREF_ANDROID_ID, Secure.getString(context.getContentResolver(), Secure.ANDROID_ID));
		editor.commit();
	}

	/**
	 * Retrieve the Android ID
	 * 
	 * @return AndroidID or null
	 */
	public String retrieveAndroidID()
	{
		return preferences.getString(PREF_ANDROID_ID, null);
	}

	/**
	 * Find and save the hardware serial number to the preferences
	 */
	private void saveHardwareSerialNumber()
	{
		SharedPreferences.Editor editor = preferences.edit();
		final String serial = Build.SERIAL;
		if(serial.equalsIgnoreCase("unknown"))
			editor.putString(PREF_HARWARE_SERIAL, null);
		else
			editor.putString(PREF_HARWARE_SERIAL, serial);
		editor.commit();
	}

	/**
	 * Retrieve the Hardware Serial Number
	 * 
	 * @return serial number or null
	 */
	public String retrieveHardwareSerialNumber()
	{
		return preferences.getString(PREF_HARWARE_SERIAL, null);
	}

	/**
	 * A method to compute and store a valid device ID
	 */
	private void computeDeviceID()
	{
		String deviceId = null;

		if(retrieveIMEI() != null)
			deviceId = retrieveIMEI();
		else if(retrieveWiFiMACAddress() != null)
			deviceId = retrieveWiFiMACAddress();
		else if(retrieveBluetoothMACAddress() != null)
			deviceId = retrieveBluetoothMACAddress();
		else if(retrieveAndroidID() != null)
			deviceId = retrieveAndroidID();
		else if(retrieveHardwareSerialNumber() != null)
			deviceId = retrieveHardwareSerialNumber();

		saveDeviceID(deviceId);
	}

	/**
	 * Save Collected Info to the preferences
	 * 
	 * @param deviceID
	 */
	private void saveDeviceID(String deviceID)
	{
		SharedPreferences.Editor editor = preferences.edit();

		editor.putString(PREF_DEVICE_ID, deviceID);
		editor.commit();
	}

	/**
	 * Retrieve Collected Info from the preferences or null
	 * 
	 * @return
	 */
	private String getDeviceID()
	{
		return preferences.getString(PREF_DEVICE_ID, null);
	}

	/**
	 * Returns a 32 bit String hash code based on assembled device information
	 * 
	 * @return the hash code
	 * @see java.lang.String#hashCode()
	 */
	public int getIntHash()
	{
		return getDeviceInfo().hashCode();
	}

	/**
	 * Returns a 64 bit String hash code based on assembled device information
	 * 
	 * Implementation adapted from String.hashCode() See: http://stackoverflow.com/a/1660613/1084488
	 * 
	 * @return the hash code
	 * @see java.lang.String#hashCode()
	 */
	public long getLongHash()
	{
		String info = getDeviceID();
		long hash = PRIME;
		for(char c : info.toCharArray())
			hash = 31 * hash + c;
		return hash;
	}

	/**
	 * Returns a 32 bit unsigned CRC hash code based on assembled device information
	 * 
	 * @return the hash code
	 * @see <a href="http://en.wikipedia.org/wiki/Cyclic_redundancy_check">http://en.wikipedia.org/wiki/Cyclic_redundancy_check</a>
	 */
	public long getCRC32Hash()
	{
		CRC32 crc32 = new CRC32();
		crc32.update(getDeviceID().getBytes());
		return crc32.getValue();
	}

	/**
	 * Returns a 128 bit MD5 hash code based on assembled device information
	 * 
	 * @return the hash code
	 * @see <a href="http://en.wikipedia.org/wiki/MD5">http://en.wikipedia.org/wiki/MD5</a>
	 */
	public BigInteger getMD5Hash()
	{
		return Hashing.getMD5Hash(getDeviceID().getBytes());
	}

	/**
	 * Print the Preferences for debugging
	 */
	public void printPreferences()
	{
		Debug.d("------------------------------------");
		Debug.d("retrieveIMEI(): " + retrieveIMEI());
		Debug.d("retrieveWiFiMACAddress(): " + retrieveWiFiMACAddress());
		Debug.d("retrieveBluetoothMACAddress(): " + retrieveBluetoothMACAddress());
		Debug.d("retrieveAndroidID(): " + retrieveAndroidID());
		Debug.d("retrieveHardwareSerialNumber(): " + retrieveHardwareSerialNumber());
		Debug.d("getDeviceID(): " + getDeviceID());
		Debug.d("------------------------------------");
	}
}
