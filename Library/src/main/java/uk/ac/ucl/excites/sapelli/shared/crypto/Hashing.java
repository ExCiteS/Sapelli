/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.shared.crypto;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Hash algorithms
 * 
 * @author mstevens
 * 
 */
public final class Hashing
{
	
	private Hashing() { } // should not be instantiated
	
	static protected HashResult digest(MessageDigest md, InputStream input) throws IOException
	{
		md.reset();
		byte[] bff = new byte[4096];
		int count = 0;
		while(input.available() > 0)
		{
			int read = input.read(bff, 0, input.available());
			md.update(bff, 0, read);
			count += read;
		}
		return new HashResult(md.digest(), count);
	}
	
	public static HashResult digest(String algorithm, InputStream input)
	{
		MessageDigest md = null;
		if(algorithm == CRC16Digest.NAME)
			md = new CRC16Digest(); // TODO remove this once we've registered CRC16 with the system
		else
			try
			{
				md = MessageDigest.getInstance(algorithm);
			}
			catch(NoSuchAlgorithmException ex)
			{
				System.err.println("Cannot get hash algorithm \'" + algorithm + "\': " + ex.getLocalizedMessage());
				return null;
			}
		// Digest:
		try
		{
			return digest(md, input);
		}
		catch(IOException e)
		{
			System.err.println("Error on digesting");
			e.printStackTrace(System.err);
			return null;
		}
	}

	/**
	 * Computes a 256 bit SHA-2 hash code from the provided data
	 * 
	 * @param data
	 * @return
	 */
	static public byte[] getSHA256Hash(byte[] data)
	{
		HashResult result = digest("SHA-256", new ByteArrayInputStream(data));
		return result != null ? result.hash : null;
	}
	
	/**
	 * Computes a 128 bit MD5 hash code from the provided data
	 * 
	 * @return the hash code as byte[]
	 * @see <a href="http://en.wikipedia.org/wiki/MD5">http://en.wikipedia.org/wiki/MD5</a>
	 */
	static public byte[] getMD5HashBytes(byte[] data)
	{
		HashResult result = digest("MD5", new ByteArrayInputStream(data));
		return result != null ? result.hash : null;
	}

	/**
	 * Computes a 128 bit MD5 hash code from the provided data
	 * 
	 * @return the hash code
	 * @see <a href="http://en.wikipedia.org/wiki/MD5">http://en.wikipedia.org/wiki/MD5</a>
	 */
	static public BigInteger getMD5HashBigInt(byte[] data)
	{
		HashResult result = digest("MD5", new ByteArrayInputStream(data));
		if(result == null)
			return BigInteger.ZERO;
		else
			return new BigInteger(result.hash);
	}
	
	static public int getCRC16Hash(byte[] byteArray)
	{
		HashResult result = digest(CRC16Digest.NAME, new ByteArrayInputStream(byteArray));
		if(result == null)
			return 0;
		else
			return (result.hash[0] & 0xFF) << 8 | (result.hash[1] & 0xFF);
	}
	
	/**
	 * @author mstevens
	 */
	static public class HashResult
	{
		
		private final byte[] hash;
		private final int inputLength;
		
		/**
		 * @param hash
		 * @param inputLength
		 */
		public HashResult(byte[] hash, int inputLength)
		{
			this.hash = hash;
			this.inputLength = inputLength;
		}

		/**
		 * @return the hash
		 */
		public byte[] getHash()
		{
			return hash;
		}

		/**
		 * @return the inputLength
		 */
		public int getInputLength()
		{
			return inputLength;
		}
		
	}

}
