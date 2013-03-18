package uk.ac.ucl.excites.transmission.sms;

import uk.ac.ucl.excites.transmission.util.Cryptography;

/**
 * @author julia, mstevens
 *
 */
public class SMSReceiver
{
	
	private static final String DEFAULT_PASSWORD = "ExCiteSWC1E6BT";

	private String phoneNumber;
	
	private boolean encryt;
	private byte[] hashedPassword;
	private byte[] rehashedPassword;
	private int smsMode = SMSTransmission.SMS_MODE_BINARY; //binary is default (for now)
	
	private int nextTransmissionID = 0;
	
	private boolean senderIntroduced;
	private long timeReceived;
	private long timeSent;
	
	public SMSReceiver()
	{
		
	}	
	
	/**
	 * @return the phoneNumber
	 */
	public String getPhoneNumber()
	{
		return phoneNumber;
	}
	
	
	public void setPassword(String password)
	{
		//Do not store the password itself!
		hashedPassword = Cryptography.getSHA256Hash(password);
		rehashedPassword = Cryptography.getSHA256Hash(hashedPassword);
	}

	/**
	 * @return the senderIntroduced
	 */
	public boolean isSenderIntroduced()
	{
		return senderIntroduced;
	}

	/**
	 * @return the timeReceived
	 */
	public long getTimeReceived() {
		return timeReceived;
	}

	/**
	 * @return the timeSent
	 */
	public long getTimeSent() {
		return timeSent;
	}

	/**
	 * @return the smsMode
	 */
	public int getSmsMode()
	{
		return smsMode;
	}

}
