/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextMessage;

/**
 * @author mstevens
 *
 */
public abstract class Receiver
{

	private TransmissionStore receivedTransmissionStore;
	private TransmissionClient client;
	
	/**
	 * 
	 */
	public Receiver(TransmissionStore receivedTransmissionStore, TransmissionClient client)
	{
		this.receivedTransmissionStore = receivedTransmissionStore;
	}
	
	public void receive(BinaryMessage binSms)
	{
//		BinarySMSTransmission transmission = receivedTransmissionStore.retrieveBinarySMSTransmission(binSms.getSender(), false, 0, binSms.getPayloadHash());
//		if(transmission == null)
//			transmission = new BinarySMSTransmission(binSms.getSender(), client, binSms);
		
	}
	
	public void receive(TextMessage txtSms)
	{
		
	}
	
	public void receive(HTTPTransmission httpTransmission)
	{
		
	}

	public abstract boolean deleteTransmissionUponDecoding();
	
}
