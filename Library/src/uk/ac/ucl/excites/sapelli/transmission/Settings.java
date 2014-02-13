/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.CompressionMode;
import uk.ac.ucl.excites.sapelli.transmission.crypto.*;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent;

/**
 * @author mstevens
 *
 */
public class Settings
{

	//STATICS--------------------------------------------------------
	public static enum SMSMode
	{
		BINARY,
		TEXT
	}
	
	static public final CompressionMode DEFAULT_COMPRESSION_MODE = CompressionMode.GZIP;
	static public final boolean DEFAULT_ENCRYPT = false;
	static public final String DEFAULT_PASSWORD = "ExCiteSWC1E6BT";
	static public final SMSMode DEFAULT_SMS_MODE = SMSMode.BINARY;
	
	static public final boolean DEFAULT_DROPBOX_UPLOAD = false;
	static public final boolean DEFAULT_DROPBOX_ALLOW_MOBILE_DATA = false;
	static public final boolean DEFAULT_DROPBOX_ALLOW_ROAMING = false;
	
	static public final boolean DEFAULT_HTTP_UPLOAD = false;
	static public final boolean DEFAULT_HTTP_ALLOW_MOBILE_DATA = true;
	static public final boolean DEFAULT_HTTP_ALLOW_ROAMING = false;
	
	static public final boolean DEFAULT_SMS_UPLOAD = false;
	static public final boolean DEFAULT_SMS_ALLOW_ROAMING = false;
	
	//DYNAMICS-------------------------------------------------------
	
	//General--------------------------
	protected CompressionMode compressionMode;
	protected boolean encrypt;
	protected byte[] encryptionKey;
	protected byte[] encryptionSalt;
	protected byte[] encryptionKeyHash;

	//Dropbox--------------------------
	protected boolean dropboxUpload;
	protected boolean dropboxAllowMobileData;
	protected boolean dropboxAllowRoaming;
	
	//HTTP-----------------------------
	protected boolean httpUpload;
	protected String serverAddress;
	protected boolean httpAllowMobileData;
	protected boolean httpAllowRoaming;
	
	//SMS------------------------------
	protected boolean smsUpload;
	protected boolean smsAllowRoaming;
	protected SMSMode smsMode;
	// Sending side:
	protected SMSAgent smsRelay;
	protected boolean smsIntroductionSent;
	// Receiving side:
	protected List<SMSAgent> smsApprovedSenders;
	
	public Settings()
	{
		compressionMode = DEFAULT_COMPRESSION_MODE;
		encrypt = DEFAULT_ENCRYPT;
		setPassword(DEFAULT_PASSWORD);
		dropboxUpload = DEFAULT_DROPBOX_UPLOAD;
		dropboxAllowMobileData = DEFAULT_DROPBOX_ALLOW_MOBILE_DATA;
		dropboxAllowRoaming = DEFAULT_DROPBOX_ALLOW_ROAMING;
		httpUpload = DEFAULT_HTTP_UPLOAD;
		httpAllowMobileData = DEFAULT_HTTP_ALLOW_MOBILE_DATA;
		httpAllowRoaming = DEFAULT_HTTP_ALLOW_ROAMING;
		smsUpload = DEFAULT_SMS_UPLOAD;
		smsAllowRoaming = DEFAULT_SMS_ALLOW_ROAMING;
		smsMode = DEFAULT_SMS_MODE;
		smsIntroductionSent = false;
		smsApprovedSenders = new ArrayList<SMSAgent>();
	}

	/**
	 * Copy contructor
	 * 
	 * @param another
	 */
	public Settings(Settings another)
	{
		compressionMode = another.compressionMode;
		encrypt = another.encrypt;
		encryptionKey = another.encryptionKey;
		encryptionSalt = another.encryptionSalt;
		encryptionKeyHash = another.encryptionKeyHash;
		dropboxUpload = another.dropboxUpload;
		dropboxAllowMobileData = another.dropboxAllowMobileData;
		dropboxAllowRoaming = another.dropboxAllowRoaming;
		httpUpload = another.httpUpload;
		httpAllowMobileData = another.httpAllowMobileData;
		httpAllowRoaming = another.httpAllowRoaming;
		smsUpload = another.smsUpload;
		smsAllowRoaming = another.smsAllowRoaming;
		smsMode = another.smsMode;
		smsIntroductionSent = another.smsIntroductionSent;
		smsApprovedSenders = new ArrayList<SMSAgent>(another.smsApprovedSenders);
	}
	
	/**
	 * @return the compressionMode
	 */
	public CompressionMode getCompressionMode()
	{
		return compressionMode;
	}

	/**
	 * @param compressionMode the compressionMode to set
	 */
	public void setCompressionMode(CompressionMode compressionMode)
	{
		this.compressionMode = compressionMode;
	}

	/**
	 * @return the encrypt
	 */
	public boolean isEncrypt()
	{
		return encrypt;
	}

	/**
	 * @param encrypt the encrypt to set
	 */
	public void setEncrypt(boolean encrypt)
	{
		this.encrypt = encrypt;
	}
	
	public void setPassword(String password)
	{
		//Do not store the password itself!
		SecureKeyGenerator keygen = new SecureKeyGenerator(password, 256);
		encryptionKey = keygen.getKey();
		encryptionSalt = keygen.getSalt();
		encryptionKeyHash = Hashing.getSHA256Hash(encryptionKey);
		//hashedPassword = Cryptography.getSHA256Hash(password.trim());
		//rehashedPassword = Cryptography.getSHA256Hash(hashedPassword);
	}
	
	/**
	 * @return the smsMode
	 */
	public SMSMode getSMSMode()
	{
		return smsMode;
	}

	/**
	 * @param smsMode the smsMode to set
	 */
	public void setSMSMode(SMSMode smsMode)
	{
		this.smsMode = smsMode;
	}

	/**
	 * @return the smsRelay
	 */
	public SMSAgent getSMSRelay()
	{
		return smsRelay;
	}

	/**
	 * @param smsRelay the smsRelay to set
	 */
	public void setSMSRelay(SMSAgent smsRelay)
	{
		this.smsRelay = smsRelay;
	}

	/**
	 * @return the smsIntroductionSent
	 */
	public boolean isSMSIntroductionSent()
	{
		return smsIntroductionSent;
	}

	/**
	 * @param smsIntroductionSent the smsIntroductionSent to set
	 */
	public void setSMSIntroductionSent(boolean smsIntroductionSent)
	{
		this.smsIntroductionSent = smsIntroductionSent;
	}

	/**
	 * @return the smsApprovedSenders
	 */
	public List<SMSAgent> getSMSApprovedSenders()
	{
		return smsApprovedSenders;
	}

	/**
	 * @return the dropboxUpload
	 */
	public boolean isDropboxUpload()
	{
		return dropboxUpload;
	}

	/**
	 * @param dropboxUpload the dropboxUpload to set
	 */
	public void setDropboxUpload(boolean dropboxUpload)
	{
		this.dropboxUpload = dropboxUpload;
	}

	/**
	 * @return the httpUpload
	 */
	public boolean isHTTPUpload()
	{
		return httpUpload;
	}

	/**
	 * @param httpUpload the httpUpload to set
	 */
	public void setHTTPUpload(boolean httpUpload)
	{
		this.httpUpload = httpUpload;
	}

	/**
	 * @return the smsUpload
	 */
	public boolean isSMSUpload()
	{
		return smsUpload;
	}

	/**
	 * @param smsUpload the smsUpload to set
	 */
	public void setSMSUpload(boolean smsUpload)
	{
		this.smsUpload = smsUpload;
	}

	/**
	 * @return the serverAddress
	 */
	public String getServerAddress()
	{
		return serverAddress;
	}

	/**
	 * @param serverAddress the serverAddress to set
	 */
	public void setServerAddress(String serverAddress)
	{
		this.serverAddress = serverAddress;
	}

	/**
	 * @return the dropboxAllowMobileData
	 */
	public boolean isDropboxAllowMobileData()
	{
		return dropboxAllowMobileData;
	}

	/**
	 * @param dropboxAllowMobileData the dropboxAllowMobileData to set
	 */
	public void setDropboxAllowMobileData(boolean dropboxAllowMobileData)
	{
		this.dropboxAllowMobileData = dropboxAllowMobileData;
	}

	/**
	 * @return the dropboxAllowRoaming
	 */
	public boolean isDropboxAllowRoaming()
	{
		return dropboxAllowRoaming;
	}

	/**
	 * @param dropboxAllowRoaming the dropboxAllowRoaming to set
	 */
	public void setDropboxAllowRoaming(boolean dropboxAllowRoaming)
	{
		this.dropboxAllowRoaming = dropboxAllowRoaming;
	}

	/**
	 * @return the httpAllowMobileData
	 */
	public boolean isHTTPAllowMobileData()
	{
		return httpAllowMobileData;
	}

	/**
	 * @param httpAllowMobileData the httpAllowMobileData to set
	 */
	public void setHTTPAllowMobileData(boolean httpAllowMobileData)
	{
		this.httpAllowMobileData = httpAllowMobileData;
	}

	/**
	 * @return the httpAllowRoaming
	 */
	public boolean isHTTPAllowRoaming()
	{
		return httpAllowRoaming;
	}

	/**
	 * @param httpAllowRoaming the httpAllowRoaming to set
	 */
	public void setHTTPAllowRoaming(boolean httpAllowRoaming)
	{
		this.httpAllowRoaming = httpAllowRoaming;
	}

	/**
	 * @return the smsAllowRoaming
	 */
	public boolean isSMSAllowRoaming()
	{
		return smsAllowRoaming;
	}

	/**
	 * @param smsAllowRoaming the smsAllowRoaming to set
	 */
	public void setSMSAllowRoaming(boolean smsAllowRoaming)
	{
		this.smsAllowRoaming = smsAllowRoaming;
	}
	
}
