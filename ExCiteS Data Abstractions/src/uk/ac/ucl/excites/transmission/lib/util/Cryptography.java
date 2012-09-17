package uk.ac.ucl.excites.transmission.lib.util;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * @author mstevens
 *
 */
public class Cryptography
{


	public byte[] getSHA256Hash(String password)
	{
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
	
}
