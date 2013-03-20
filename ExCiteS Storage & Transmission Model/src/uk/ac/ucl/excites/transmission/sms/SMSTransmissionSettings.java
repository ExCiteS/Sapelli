/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.transmission.TransmissionSettings;

/**
 * @author mstevens
 *
 */
public class SMSTransmissionSettings extends TransmissionSettings
{

	//Statics--------------------------------------------------------
	public static enum SMSMode
	{
		BINARY,
		TEXT
	}
	
	public static final SMSMode DEFAULT_SMS_MODE = SMSMode.BINARY; 

	//Dynamics-------------------------------------------------------
	private SMSMode smsMode;
	
	//Used on sending side only:
	private SMSAgent receiver;
	private boolean introductionSent;
	private int nextTransmissionID;
	
	//Used on receiving side only:
	private List<SMSAgent> approvedSenders;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param receiver
	 */
	public SMSTransmissionSettings(SMSAgent receiver)
	{
		this(DEFAULT_SMS_MODE, receiver);
	}
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param smsMode
	 * @param receiver
	 */
	public SMSTransmissionSettings(SMSMode smsMode, SMSAgent receiver)
	{
		super(); //!!!
		this.smsMode = smsMode;
		this.receiver = receiver;
		this.introductionSent = false;
		this.nextTransmissionID = SMSTransmission.INITIAL_ID;
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 */
	public SMSTransmissionSettings()
	{
		this(DEFAULT_SMS_MODE);
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param smsMode
	 */
	public SMSTransmissionSettings(SMSMode smsMode)
	{
		super();
		this.smsMode = smsMode;
		this.approvedSenders = new ArrayList<SMSAgent>();
	}

	/**
	 * @return the smsMode
	 */
	public SMSMode getSmsMode()
	{
		return smsMode;
	}

	/**
	 * @return the introductionSent
	 */
	public boolean isIntroductionSent()
	{
		return introductionSent;
	}

	/**
	 * @param introductionSent the introductionSent to set
	 */
	public void setIntroductionSent(boolean introductionSent)
	{
		this.introductionSent = introductionSent;
	}

	/**
	 * @return the receiver
	 */
	public SMSAgent getReceiver()
	{
		return receiver;
	}

	/**
	 * @return the nextTransmissionID
	 */
	public int getNextTransmissionID()
	{
		int current = nextTransmissionID;
		//Next one after the current:
		nextTransmissionID = (SMSTransmission.ID_FIELD.fits(nextTransmissionID + 1) ? (nextTransmissionID + 1) : SMSTransmission.INITIAL_ID);
		//Return current
		return current;
	}
	
}
