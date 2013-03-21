/**
 * 
 */
package uk.ac.ucl.excites.transmission;

import uk.ac.ucl.excites.transmission.util.Cryptography;

/**
 * @author mstevens
 *
 */
public class TransmissionSettings
{

	//STATICS--------------------------------------------------------
	static public enum CompressionMode
	{
		NONE,
		HUFFMAN,
		GZIP
	}
	
	static public final CompressionMode DEFAULT_COMPRESSION_MODE = CompressionMode.HUFFMAN;
	
	static private final String DEFAULT_PASSWORD = "ExCiteSWC1E6BT";
	
	//DYNAMICS-------------------------------------------------------
	protected CompressionMode compressionMode;
	
	protected boolean encrypt;
	protected byte[] hashedPassword;
	protected byte[] rehashedPassword;
	
	public TransmissionSettings()
	{
		compressionMode = DEFAULT_COMPRESSION_MODE;
		setPassword(DEFAULT_PASSWORD);
	}
	
	public void setPassword(String password)
	{
		//Do not store the password itself!
		hashedPassword = Cryptography.getSHA256Hash(password);
		rehashedPassword = Cryptography.getSHA256Hash(hashedPassword);
	}
	
}
