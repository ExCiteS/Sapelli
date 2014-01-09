package uk.ac.ucl.excites.collector.util;

import java.math.BigInteger;
import java.util.zip.CRC32;

import uk.ac.ucl.excites.transmission.crypto.Hashing;
import uk.ac.ucl.excites.util.Debug;
import android.bluetooth.BluetoothAdapter;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.net.wifi.WifiManager;
import android.os.Build;
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

	// Statics
	private static final long PRIME = 1125899906842597L;
	private static final char SEPARATOR = '|';
	private WifiManager wifiManager;
	private BluetoothAdapter bluetoothAdapter;
	private SharedPreferences preferences;

	// Dynamics
	private Context context;

	public static final String PREFERENCES = "DEVICEID_PREFERENCES";
	private static final String PREF_DEVICEID_INFO = "DEVICEID_INFO";
	private static final String PREF_WIFI_MAC = "DEVICEID_WIFI_MAC";
	private static final String PREF_BLUETOOTH_MAC = "DEVICEID_BLUETOOTH_MAC";

	public DeviceID(Context context)
	{
		this.context = context;

		wifiManager = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
		bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
		preferences = context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);

		// Register for broadcasts on WiFi Manager state change
		IntentFilter wifiFilter = new IntentFilter(WifiManager.WIFI_STATE_CHANGED_ACTION);
		context.registerReceiver(WiFiReceiver, wifiFilter);

		// Register for broadcasts on BluetoothAdapter state change
		IntentFilter bluetoothFilter = new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED);
		context.registerReceiver(BluetoothReceiver, bluetoothFilter);
	}

	public String getDeviceInfo()
	{
		return Build.BRAND + SEPARATOR + Build.MANUFACTURER + SEPARATOR + Build.MODEL + SEPARATOR + Build.PRODUCT + SEPARATOR + Build.DEVICE + SEPARATOR
				+ Build.DISPLAY + SEPARATOR + Build.ID + SEPARATOR + Build.BOARD + SEPARATOR + Build.HARDWARE + SEPARATOR + Build.CPU_ABI + SEPARATOR
				+ Build.CPU_ABI2 + SEPARATOR + Build.VERSION.CODENAME + SEPARATOR + Build.VERSION.INCREMENTAL + SEPARATOR + Build.VERSION.SDK_INT + SEPARATOR
				+ Build.BOOTLOADER + SEPARATOR + Build.FINGERPRINT + SEPARATOR;
	}

	/**
	 * Returns the hardware serial number
	 * 
	 * @return the hardware serial number (if available, null otherwise)
	 */
	public String getHardwareSerialNumber()
	{
		return Build.SERIAL;
	}

	/**
	 * Returns the IMEI code of the device
	 * 
	 * Requires android.permission.READ_PHONE_STATE
	 * 
	 * @return the imei (can be null)
	 */
	public String getIMEI()
	{
		// IMEI
		TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
		if(tm != null)
			return tm.getDeviceId();
		return null;
	}

	/**
	 * Returns the Android ID (if available)
	 * 
	 * @return the AndroidID (can be null)
	 */
	public String getAndroidID()
	{
		return Secure.getString(context.getContentResolver(), Secure.ANDROID_ID);
	}

	/**
	 * Returns the MAC address of the Wi-Fi NIC. The method will attempt to enable the WiFi, get the MAC address and disable again the adapter.
	 * 
	 * Requires android.permission.ACCESS_WIFI_STATE
	 * 
	 * @return the mac address (can be null)
	 */
	public String getWiFiMACAddress()
	{
		// Firstly check if the address exists in the preferences
		String wifiAddress = preferences.getString(PREF_WIFI_MAC, null);

		if(wifiAddress != null)
		{
			return wifiAddress;
		}
		// else if the WiFi Manager exists and is on, try to get the address
		else if(wifiManager != null && wifiManager.isWifiEnabled())
		{
			wifiAddress = wifiManager.getConnectionInfo().getMacAddress();
			// Save WiFi Address to preferences
			saveWiFiAddress(wifiAddress);

			return wifiAddress;
		}
		// Finally, try to enable the WiFi Adapter and wait for the broadcast receiver
		else if(wifiManager != null)
		{
			wifiManager.setWifiEnabled(true);
		}
		return null;
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
	private BroadcastReceiver WiFiReceiver = new BroadcastReceiver()
	{
		@Override
		public void onReceive(Context context, Intent intent)
		{
			int extraWifiState = intent.getIntExtra(WifiManager.EXTRA_WIFI_STATE, WifiManager.WIFI_STATE_UNKNOWN);

			switch(extraWifiState)
			{
			case WifiManager.WIFI_STATE_DISABLED:
				Debug.d("WiFi Disabled");
				// Unregister the Broadcast Receiver
				context.unregisterReceiver(this);
				break;
			case WifiManager.WIFI_STATE_DISABLING:
				Debug.d("WiFi Disabling");
				break;
			case WifiManager.WIFI_STATE_ENABLED:
				Debug.d("WiFi Enabled");

				String wifiAddress = wifiManager.getConnectionInfo().getMacAddress();

				saveWiFiAddress(wifiAddress);

				// Try to disable WiFi
				wifiManager.setWifiEnabled(false);

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

	/**
	 * Returns the MAC address of the Bluetooth NIC. The method will attempt to enable the Bluetooth, get the address and disable again the adapter.
	 * 
	 * Requires android.permission.BLUETOOTH
	 * 
	 * @return the mac address (can be null)
	 */
	public String getBluetoothMACAddress()
	{
		// Firstly check if the address exists in the preferences
		String bluetoothAddress = preferences.getString(PREF_BLUETOOTH_MAC, null);

		if(bluetoothAddress != null)
		{
			return bluetoothAddress;
		}
		// else if the Bluetooth Adapter exists and is on, try to get the address
		else if(bluetoothAdapter != null && bluetoothAdapter.isEnabled())
		{
			bluetoothAddress = bluetoothAdapter.getAddress();
			// Save Bluetooth Address to preferences
			saveBluetoothAdress(bluetoothAddress);

			return bluetoothAddress;
		}
		// Finally, try to enable the Bluetooth Adapter and wait for the broadcast receiver
		else if(wifiManager != null)
		{
			bluetoothAdapter.enable();
		}
		return null;
	}

	/**
	 * Save the Bluetooth MAC Address to the preferences
	 * 
	 * @param bluetoothAddress
	 */
	private void saveBluetoothAdress(String bluetoothAddress)
	{
		SharedPreferences.Editor editor = preferences.edit();
		editor.putString(PREF_BLUETOOTH_MAC, bluetoothAddress);
		editor.commit();
	}

	/**
	 * Broadcast receiver to handle Bluetooth states
	 */
	private final BroadcastReceiver BluetoothReceiver = new BroadcastReceiver()
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
					// Unregister the Broadcast Receiver
					context.unregisterReceiver(this);
					break;
				case BluetoothAdapter.STATE_TURNING_OFF:
					Debug.d("Turning Bluetooth off...");
					break;
				case BluetoothAdapter.STATE_ON:

					Debug.d("Bluetooth on");
					String bluetoothAddress = bluetoothAdapter.getAddress();

					saveBluetoothAdress(bluetoothAddress);

					// Try to disable Bluetooth
					bluetoothAdapter.disable();

					break;
				case BluetoothAdapter.STATE_TURNING_ON:
					Debug.d("Turning Bluetooth on...");
					break;
				}
			}
		}
	};

	/**
	 * A method to return a valid device ID by with some fallback mechanisms. <br/>
	 * <br/>
	 * At the beginning the method tries to return the IMEI (or MEID, ESN, IMSI) if the device is a mobile. If it fails: <br/>
	 * Then returns the MAC address of the Wi-Fi NIC. If the Wi-Fi is off, it will turn it on without any user interaction and save the NIC address to the
	 * preferences for future reference. If it fails:<br/>
	 * Then returns the MAC address of the Bluetooth NIC. If the Bluetooth is off, it will turn it on without any user interaction and save the NIC address to
	 * the preferences for future reference. If it fails:<br/>
	 * Then returns the Android ID and if it fails:<br/>
	 * Finally it returns the Hardware Serial Number
	 * 
	 * @return a deviceID or null if none of them works
	 */
	private String collectInfo()
	{
		final String retrieveInfo = retrieveInfo();
		if(retrieveInfo != null && !retrieveInfo.isEmpty())
		{
			return retrieveInfo;
		}
		else
		{
			String IMEI = getIMEI();

			if(IMEI != null && !IMEI.isEmpty())
			{
				saveInfo(IMEI);
				return IMEI;
			}

			String wiFiMACAddress = getWiFiMACAddress();

			if(wiFiMACAddress != null && !wiFiMACAddress.isEmpty())
			{
				saveInfo(wiFiMACAddress);
				return wiFiMACAddress;
			}

			String bluetoothMACAddress = getBluetoothMACAddress();

			if(bluetoothMACAddress != null && !bluetoothMACAddress.isEmpty())
			{
				saveInfo(bluetoothMACAddress);
				return bluetoothMACAddress;
			}

			String androidID = getAndroidID();

			if(androidID != null && !androidID.isEmpty())
			{
				saveInfo(androidID);
				return androidID;
			}

			String hardwareSerialNumber = getHardwareSerialNumber();

			if(hardwareSerialNumber != null && !hardwareSerialNumber.isEmpty())
			{
				saveInfo(hardwareSerialNumber);
				return hardwareSerialNumber;
			}
		}

		return null;
	}

	/**
	 * Save Collected Info to the preferences
	 * 
	 * @param collectedInfo
	 */
	private void saveInfo(String collectedInfo)
	{
		SharedPreferences.Editor editor = preferences.edit();

		editor.putString(PREF_DEVICEID_INFO, collectedInfo);
		editor.commit();
	}

	/**
	 * Retrieve Collected Info from the preferences or null
	 * 
	 * @return
	 */
	private String retrieveInfo()
	{
		return preferences.getString(PREF_DEVICEID_INFO, null); // the default value is 0
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
	 * Implementation adapted from String.hashCode()
	 * See: http://stackoverflow.com/a/1660613/1084488
	 * 
	 * @return the hash code
	 * @see java.lang.String#hashCode()
	 */
	public long getLongHash()
	{
		String info = collectInfo();
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
		crc32.update(collectInfo().getBytes());
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
		return Hashing.getMD5Hash(collectInfo().getBytes());
	}

	/**
	 * Checks if the device returns a valid DeviceID
	 * 
	 * @return
	 */
	public boolean hasDeviceID()
	{
		final String collectInfo = collectInfo();
		return(collectInfo != null && !collectInfo.isEmpty());
	}

	/**
	 * Print the Device Info for debugging
	 */
	public void printDeviceInfo()
	{
		Debug.d("getDeviceInfo(): " + getDeviceInfo());
		Debug.d("collectInfo(): " + collectInfo());
		Debug.d("getHardwareSerialNumber(): " + getHardwareSerialNumber());
		Debug.d("getIMEI(): " + getIMEI());
		Debug.d("getAndroidID(): " + getAndroidID());
		Debug.d("getWiFiMACAddress(): " + getWiFiMACAddress());
		Debug.d("getBluetoothMACAddress(): " + getBluetoothMACAddress());
		Debug.d("getCRC32Hash(): " + getCRC32Hash());
	}
}
