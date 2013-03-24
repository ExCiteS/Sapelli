/**
 * 
 */
package uk.ac.ucl.excites.transmission;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.util.Cryptography;

/**
 * @author mstevens
 *
 */
public class Settings
{

	//STATICS--------------------------------------------------------
	static public enum CompressionMode
	{
		NONE,
		HUFFMAN,
		GZIP
	}
	
	public static enum SMSMode
	{
		BINARY,
		TEXT
	}
	
	static public final CompressionMode DEFAULT_COMPRESSION_MODE = CompressionMode.HUFFMAN;
	static public final boolean DEFAULT_ENCRYPT = false;
	static public final String DEFAULT_PASSWORD = "ExCiteSWC1E6BT";
	static public final SMSMode DEFAULT_SMS_MODE = SMSMode.BINARY;
	
	static public final boolean DEFAULT_DROPBOX_UPLOAD = false;
	static public final boolean DEFAULT_HTTP_UPLOAD = false;
	static public final boolean DEFAULT_SMS_UPLOAD = false;
	
	static public final boolean DEFAULT_ALLOW_MOBILE_DATA = false;
	static public final boolean DEFAULT_ALLOW_ROAMING = false;
	
	//DYNAMICS-------------------------------------------------------
	
	//General--------------------------
	protected CompressionMode compressionMode;
	protected boolean encrypt;
	protected byte[] hashedPassword;
	protected byte[] rehashedPassword;
	
	protected boolean dropboxUpload;
	protected boolean httpUpload;
	protected boolean smsUpload;
	
	protected boolean allowMobileData;
	protected boolean allowRoaming;
	
	//HTTP specific--------------------
	protected String serverAddress;
	
	//SMS specific---------------------
	protected SMSMode smsMode;
	
	//Used on sending side only:
	protected SMSAgent smsRelay;
	protected boolean smsIntroductionSent;
	protected int smsNextTransmissionID;
	
	//Used on receiving side only:
	protected List<SMSAgent> smsApprovedSenders;
	
	public Settings()
	{
		compressionMode = DEFAULT_COMPRESSION_MODE;
		encrypt = DEFAULT_ENCRYPT;
		setPassword(DEFAULT_PASSWORD);
		dropboxUpload = DEFAULT_DROPBOX_UPLOAD;
		httpUpload = DEFAULT_HTTP_UPLOAD;
		smsUpload = DEFAULT_SMS_UPLOAD;
		allowMobileData = DEFAULT_ALLOW_MOBILE_DATA;
		allowRoaming = DEFAULT_ALLOW_ROAMING;
		smsMode = DEFAULT_SMS_MODE;
		smsIntroductionSent = false;
		smsNextTransmissionID = SMSTransmission.INITIAL_ID;
		smsApprovedSenders = new ArrayList<SMSAgent>();
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
		hashedPassword = Cryptography.getSHA256Hash(password.trim());
		rehashedPassword = Cryptography.getSHA256Hash(hashedPassword);
	}
	
	/**
	 * @return the smsNextTransmissionID
	 */
	public int getSMSTransmissionID()
	{
		int current = smsNextTransmissionID;
		//Next one after the current:
		smsNextTransmissionID = (SMSTransmission.ID_FIELD.fits(current + 1) ? (current + 1) : SMSTransmission.INITIAL_ID);
		//Return current
		return current;
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
	 * @return the hashedPassword
	 */
	public byte[] getHashedPassword()
	{
		return hashedPassword;
	}

	/**
	 * @return the rehashedPassword
	 */
	public byte[] getRehashedPassword()
	{
		return rehashedPassword;
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
	 * @return the allowMobileData
	 */
	public boolean isAllowMobileData()
	{
		return allowMobileData;
	}

	/**
	 * @param allowMobileData the allowMobileData to set
	 */
	public void setAllowMobileData(boolean allowMobileData)
	{
		this.allowMobileData = allowMobileData;
	}

	/**
	 * @return the allowRoaming
	 */
	public boolean isAllowRoaming()
	{
		return allowRoaming;
	}

	/**
	 * @param allowRoaming the allowRoaming to set
	 */
	public void setAllowRoaming(boolean allowRoaming)
	{
		this.allowRoaming = allowRoaming;
	}
	
}
