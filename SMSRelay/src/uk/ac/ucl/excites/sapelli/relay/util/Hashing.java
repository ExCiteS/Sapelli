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

package uk.ac.ucl.excites.sapelli.relay.util;

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
	
	static public byte[] getSHA256Hash(String data)
	{
		return getSHA256Hash(data.getBytes());
	}
	
	static public byte[] getSHA256Hash(byte[] data)
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
