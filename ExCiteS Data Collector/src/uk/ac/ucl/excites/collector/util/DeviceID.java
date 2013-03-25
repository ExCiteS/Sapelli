package uk.ac.ucl.excites.collector.util;

import uk.ac.ucl.excites.transmission.crypto.Hashing;

import java.math.BigInteger;
import java.util.zip.CRC32;

import android.bluetooth.BluetoothAdapter;
import android.content.Context;
import android.net.wifi.WifiManager;
import android.os.Build;
import android.provider.Settings.Secure;
import android.telephony.TelephonyManager;


/**
 * Class that provides various ways to uniquely (or as unique as possible) identify an Android device<br/>
 * 
 * Based on: <a href="http://www.pocketmagic.net/2011/02/android-unique-device-id/#.USeA7KXvh8H">http://www.pocketmagic.net/2011/02/android-unique-device-id/#.USeA7KXvh8H</a>
 * 
 * @author mstevens
 */
public class DeviceID
{

	//Statics
	private static final long PRIME = 1125899906842597L;
	private static final char SEPARATOR = '|';

	//Dynamics
	private Context context;

	public DeviceID(Context context)
	{
		this.context = context;
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
	 * Returns the MAC address of the Wi-Fi NIC
	 * 
	 * Requires android.permission.ACCESS_WIFI_STATE
	 * 
	 * @return the mac address (can be null)
	 */
	public String getWiFiMACAddress()
	{
		WifiManager wm = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
		if(wm != null)
			return wm.getConnectionInfo().getMacAddress();
		return null;
	}

	/**
	 * Returns the MAC address of the Bluetooth NIC
	 * 
	 * Requires android.permission.BLUETOOTH
	 * 
	 * @return the mac address (can be null)
	 */
	public String getBluetoothMACAddress()
	{
		BluetoothAdapter bta = BluetoothAdapter.getDefaultAdapter();
		if(bta != null)
			return bta.getAddress();
		return null;
	}

	private String collectInfo()
	{
		return getDeviceInfo() + getHardwareSerialNumber() + getIMEI() + getWiFiMACAddress() + getBluetoothMACAddress() + getAndroidID();
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

}
