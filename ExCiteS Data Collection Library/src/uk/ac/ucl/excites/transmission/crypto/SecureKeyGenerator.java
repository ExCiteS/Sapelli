/**
 * 
 */
package uk.ac.ucl.excites.transmission.crypto;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

/**
 * @author mstevens
 *
 * @see http://stackoverflow.com/a/992413/1084488
 * @see http://stackoverflow.com/a/13438590/1084488
 * @see http://nelenkov.blogspot.co.uk/2012/04/using-password-based-encryption-on.html
 */
public class SecureKeyGenerator
{

	static private final int NUMBER_OF_ITERATIONS = 100;
	static public final int SALT_LENGTH = 8;
	
	static public byte[] GenerateSalt()
	{
	    //Generate salt:
	    SecureRandom random = new SecureRandom();
	    byte[] salt = new byte[SALT_LENGTH];
	    random.nextBytes(salt);
	    return salt;
	}
	
    private byte[] salt;
    private byte[] key;
	
	public SecureKeyGenerator(String password, int keySize)
	{
		this(password, GenerateSalt(), keySize);	    
	}
	
	public SecureKeyGenerator(String password, byte[] salt, int keySize)
	{
		//Use the given salt:
		this.salt = salt;
	    // Derive the key from the password:
		try
		{
			KeySpec keySpec = new PBEKeySpec(password.toCharArray(), salt, NUMBER_OF_ITERATIONS, keySize);
		    SecretKeyFactory keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
			key = keyFactory.generateSecret(keySpec).getEncoded();
		}
		catch(NoSuchAlgorithmException e)
		{
			System.err.println("Cannot obtain key generation algorithm: " + e.getLocalizedMessage());
			e.printStackTrace(System.err);
		}
		catch(InvalidKeySpecException e)
		{
			System.err.println("Invalid key spec: " + e.getLocalizedMessage());
			e.printStackTrace(System.err);
		}	
	}

	/**
	 * @return the salt
	 */
	public byte[] getSalt()
	{
		return salt;
	}

	/**
	 * @return the key
	 */
	public byte[] getKey()
	{
		return key;
	}
	
}
