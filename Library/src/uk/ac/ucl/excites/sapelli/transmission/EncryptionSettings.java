package uk.ac.ucl.excites.sapelli.transmission;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.transmission.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.transmission.crypto.SecureKeyGenerator;


/**
 * @author mstevens
 *
 */
public class EncryptionSettings
{

	static public final boolean DEFAULT_ALLOW_ENCRYPTION = true;
	
	static public final String DEFAULT_PASSWORD = "ExCiteSWC1E6BT";
	static public final String DEFAULT_PASSWORD_LABEL = "(Sapelli default)";
	
	protected boolean allowEncryption;
	protected final List<Key> keys;
	
	public EncryptionSettings()
	{
		allowEncryption = DEFAULT_ALLOW_ENCRYPTION;
		keys = new ArrayList<Key>();
		keys.add(new Key(DEFAULT_PASSWORD_LABEL, DEFAULT_PASSWORD));
	}
	
	/**
	 * Copy contructor
	 * 
	 * @param another
	 */
	public EncryptionSettings(EncryptionSettings another)
	{
		allowEncryption = another.allowEncryption;
		keys = new ArrayList<Key>();
		Collections.copy(keys, another.keys);
	}
	
	/**
	 * @return the allowEncryption
	 */
	public boolean isAllowEncryption()
	{
		return allowEncryption;
	}
	
	/**
	 * @param allowEncryption
	 */
	public void setAllowEncryption(boolean allowEncryption)
	{
		this.allowEncryption = allowEncryption;
	}

	/**
	 * @return the keys
	 */
	public List<Key> getKeys()
	{
		return keys;
	}

	/**
	 * @param label
	 * @param password
	 */
	public void addKey(String label, String password)
	{
		keys.add(new Key(label, password));
	}
	
	/**
	 * @param key
	 */
	public void removeKey(Key key)
	{
		keys.remove(key);
	}

	/**
	 * @author mstevens
	 *
	 */
	public class Key
	{
		
		private String label;
		private byte[] encryptionKey;
		private byte[] encryptionSalt;
		private byte[] encryptionKeyHash;
		
		public Key(String label, String password)
		{
			this.label = label;
			//Do never store the password itself!
			SecureKeyGenerator keygen = new SecureKeyGenerator(password, 256);
			encryptionKey = keygen.getKey();
			encryptionSalt = keygen.getSalt();
			encryptionKeyHash = Hashing.getSHA256Hash(encryptionKey);
			//hashedPassword = Cryptography.getSHA256Hash(password.trim());
			//rehashedPassword = Cryptography.getSHA256Hash(hashedPassword);
		}

		/**
		 * @return the label
		 */
		public String getLabel()
		{
			return label;
		}

		/**
		 * @return the encryptionKey
		 */
		public byte[] getEncryptionKey()
		{
			return encryptionKey;
		}

		/**
		 * @return the encryptionSalt
		 */
		public byte[] getEncryptionSalt()
		{
			return encryptionSalt;
		}

		/**
		 * @return the encryptionKeyHash
		 */
		public byte[] getEncryptionKeyHash()
		{
			return encryptionKeyHash;
		}
		
	}

}
