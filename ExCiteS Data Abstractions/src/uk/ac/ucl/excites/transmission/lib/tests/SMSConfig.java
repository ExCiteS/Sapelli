/**
 * 
 */
package uk.ac.ucl.excites.transmission.lib.tests;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * @author mstevens
 * 
 */
public class SMSConfig
{

	private String phoneNumber;
	private boolean introductionSent = false;

	private byte[] key;

	public SMSConfig(String phoneNumber, String password)
	{
		this.key = getHash(password);
	}

	private byte[] getHash(String password)
	{
		MessageDigest digest = null;
		try
		{
			digest = MessageDigest.getInstance("SHA-256");
		}
		catch(NoSuchAlgorithmException e)
		{
			e.printStackTrace();
		}
		digest.reset();
		return digest.digest(password.getBytes());
	}

}
