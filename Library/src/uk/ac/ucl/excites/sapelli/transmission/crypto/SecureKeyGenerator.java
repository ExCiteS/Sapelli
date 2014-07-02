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

package uk.ac.ucl.excites.sapelli.transmission.crypto;

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
