package uk.ac.ucl.excites.encryption;

import java.security.Key;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Security;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import org.spongycastle.jce.provider.BouncyCastleProvider;
import org.spongycastle.util.encoders.Base64;

public class AESencrp {
	private final String ALGO = "AES";
	private byte[] keyValue;

	static {
		Security.addProvider(new BouncyCastleProvider());
	}

	public byte[] encrypt(byte[] Data) throws Exception {
		Key key = generateKey();
		Cipher c = Cipher.getInstance(ALGO);
		c.init(Cipher.ENCRYPT_MODE, key);
		byte[] encVal = c.doFinal(Data);
		new Base64();
		byte[] encryptedValue = Base64.encode(encVal);
		return encryptedValue;
	}

	public byte[] decrypt(byte[] Data) throws Exception {
		Key key = generateKey();
		Cipher c = Cipher.getInstance(ALGO);
		c.init(Cipher.DECRYPT_MODE, key);
		byte[] decordedValue = Base64.decode(Data);
		byte[] decValue = c.doFinal(decordedValue);
//		String decryptedValue = new String(decValue);
		return decValue;
	}

	public void getSHA256Hash(String password) {
		MessageDigest digest = null;
		try {
			digest = MessageDigest.getInstance("SHA-256");
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		}
		digest.reset();

		this.keyValue = digest.digest(password.getBytes());
	}

	private Key generateKey() throws Exception {
		Key key = new SecretKeySpec(keyValue, "AES");
		return key;
	}

}
