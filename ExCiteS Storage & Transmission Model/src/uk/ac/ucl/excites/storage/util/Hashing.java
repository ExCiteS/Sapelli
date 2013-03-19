package uk.ac.ucl.excites.storage.util;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * @author mstevens
 *
 */
public final class Hashing
{
	
	private Hashing() { } //should not be instantiated
	
	/**
	 * Returns a 128 bit MD5 hash code based on the provided data
	 * 
	 * @return the hash code
	 * @see <a href="http://en.wikipedia.org/wiki/MD5">http://en.wikipedia.org/wiki/MD5</a>
	 */
	static public BigInteger getMD5Hash(byte[] data)
	{
		try
		{
			MessageDigest md5 = MessageDigest.getInstance("MD5");
			md5.update(data);
			return new BigInteger(md5.digest());
		}
		catch(NoSuchAlgorithmException e)
		{
			System.err.println("Could not get MD5 implementation: " + e.getLocalizedMessage());
			e.printStackTrace();
			return BigInteger.ZERO;
		}
	}

}
