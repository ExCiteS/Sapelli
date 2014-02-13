/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.sms;

import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;

/**
 * @author mstevens
 *
 */
public class SMSTransmissionID
{
	
	static public final int ID_SIZE_BITS = Byte.SIZE;
	static public final int INITIAL_ID = 0;
	static public final IntegerRangeMapping FIELD = IntegerRangeMapping.ForSize(INITIAL_ID, ID_SIZE_BITS);

	private int nextTransmissionID;

	public SMSTransmissionID()
	{
		nextTransmissionID = INITIAL_ID;
	}
	
	/**
	 * @return the smsNextTransmissionID
	 */
	public int getNewID()
	{
		int current = nextTransmissionID;
		//Next one after the current:
		nextTransmissionID = (FIELD.fits(current + 1) ? (current + 1) : INITIAL_ID);
		//Return current
		return current;
	}

}
