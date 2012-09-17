package uk.ac.ucl.excites.transmission.lib.tests;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * 
 */

/**
 * @author mstevens
 *
 */
public class Hashing {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Hashing me = new Hashing();
		String plainpass = "ExCiteS";
		System.out.println("\"" + plainpass + "\" becomes: " + me.bin2hex(me.getHash("asdf")));
	}

	public byte[] getHash(String password) {
	       MessageDigest digest=null;
	    try {
	        digest = MessageDigest.getInstance("SHA-256");
	    } catch (NoSuchAlgorithmException e1) {
	        // TODO Auto-generated catch block
	        e1.printStackTrace();
	    }
	       digest.reset();
	       return digest.digest(password.getBytes());
	}
	
	static String bin2hex(byte[] data)
	{
	    return String.format("%0" + (data.length*2) + "X", new BigInteger(1, data));
	}
	
}
