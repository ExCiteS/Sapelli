package uk.ac.ucl.excites.transmission.util;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * @author mstevens
 * 
 */
public class Cryptography
{

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
			System.err.println("Cannot get hash algorithm: " + ex.getLocalizedMessage());
			ex.printStackTrace(System.err);
			return null;
		}
		digest.reset();
		return digest.digest(data);
	}

}
