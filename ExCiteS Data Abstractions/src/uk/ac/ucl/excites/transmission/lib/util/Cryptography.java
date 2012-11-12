package uk.ac.ucl.excites.transmission.lib.util;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import android.util.Log;

/**
 * @author mstevens
 * 
 */
public class Cryptography
{

	public static final String TAG = "Crypt";
	
	public static byte[] getSHA256Hash(String data)
	{
		return getSHA256Hash(data.getBytes());
	}
	
	public static byte[] getSHA256Hash(byte[] data)
	{
		MessageDigest digest = null;
		try
		{
			digest = MessageDigest.getInstance("SHA-256");
		}
		catch(NoSuchAlgorithmException ex)
		{
			Log.e(TAG, "Cannot get hash algorithm", ex);
		}
		digest.reset();
		return digest.digest(data);
	}

}
