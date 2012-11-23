package uk.ac.ucl.excites.gcmencryption;

import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.Security;

import org.spongycastle.crypto.InvalidCipherTextException;
import org.spongycastle.crypto.engines.AESEngine;
import org.spongycastle.crypto.modes.GCMBlockCipher;
import org.spongycastle.crypto.params.AEADParameters;
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.jce.provider.BouncyCastleProvider;



public class GCMencryp {
	private static final SecureRandom rand = new SecureRandom();
	private static final int MAC_SIZE = 128;
	private static final byte[] IVvector = new byte[4];
	private static final byte[] associatedText = "Some associated Text"
			.getBytes(Charset.forName("UTF-8"));

	static {
		Security.addProvider(new BouncyCastleProvider());
	}

	static {
		rand.nextBytes(IVvector);
	}

	
	/**
	 * Returns the ciphertext encrypted from the given plaintext and AEAD
	 * parameters.
	 */
	public byte[] encrypt(byte[] pwHash, byte[] plaintext)
			throws InvalidCipherTextException {
		AEADParameters params = new AEADParameters(generateKey(pwHash),
				MAC_SIZE, IVvector, associatedText);
		GCMBlockCipher gcm = new GCMBlockCipher(new AESEngine());
		gcm.init(true, params);
		int outsize = gcm.getOutputSize(plaintext.length);
		byte[] out = new byte[outsize];
		int offOut = gcm.processBytes(plaintext, 0, plaintext.length, out, 0);
		gcm.doFinal(out, offOut);
		return out;
	}

	/**
	 * Returns the plaintext decrypted from the given ciphertext and AEAD
	 * parameters.
	 */
	public byte[] decrypt(byte[] pwHash, byte[] ciphertext)
			throws InvalidCipherTextException {
		AEADParameters params = new AEADParameters(generateKey(pwHash),
				MAC_SIZE, IVvector, associatedText);
		GCMBlockCipher gcm = new GCMBlockCipher(new AESEngine());
		gcm.init(false, params);
		int outsize = gcm.getOutputSize(ciphertext.length);
		byte[] out = new byte[outsize];
		int offOut = gcm.processBytes(ciphertext, 0, ciphertext.length, out, 0);
		gcm.doFinal(out, offOut);
		return out;
	}

	public byte[] getSHA256Hash(String password) {
		MessageDigest digest = null;
		try {
			digest = MessageDigest.getInstance("SHA-256");
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		}
		digest.reset();

		return digest.digest(password.getBytes());
	}

	public KeyParameter generateKey(byte[] pwHash) {
		return new KeyParameter(pwHash);
	}

}