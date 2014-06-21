/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.util;

import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSTransmission;

/**
 * @author mstevens
 *
 */
public class IncompleteTransmissionException extends Exception
{

	private static final long serialVersionUID = 1L;

	private Transmission transmission;
	
	public IncompleteTransmissionException(Transmission transmission)
	{
		this(transmission, "Incomplete transmission");
	}
	
	public IncompleteTransmissionException(SMSTransmission transmission)
	{
		this(transmission, "Incomplete transmission, " + (transmission.getTotalNumberOfParts() - transmission.getCurrentNumberOfParts()) + "/" + transmission.getTotalNumberOfParts() + " parts missing");
	}

	/**
	 * @param detailMessage
	 */
	public IncompleteTransmissionException(Transmission transmission, String detailMessage)
	{
		super(detailMessage);
		this.transmission = transmission;
	}

	/**
	 * @return the transmission
	 */
	public Transmission getTransmission()
	{
		return transmission;
	}

}
