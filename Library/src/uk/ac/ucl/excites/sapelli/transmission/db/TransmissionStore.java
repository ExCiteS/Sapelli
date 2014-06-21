package uk.ac.ucl.excites.sapelli.transmission.db;

import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextSMSTransmission;

public abstract class TransmissionStore implements Store
{

	public TransmissionStore()
	{
		// TODO Auto-generated constructor stub
	}
	
	public abstract void storeTransmission(BinarySMSTransmission binaraySMSTransmission);
	
	public abstract void storeTransmission(TextSMSTransmission binaraySMSTransmission);
	
	public abstract void storeTransmission(HTTPTransmission httpTransmission);
	
	public abstract List<Integer> retrieveTransmissionIDs(int type, int payloadType, int payloadHash);
	
	public BinarySMSTransmission retrieveBinarySMSTransmission(SMSAgent correspondent, boolean sent, int payloadType, int payloadHash)
	{
		// throw special exception when not unique		
		return null;
	}
	
	public TextSMSTransmission retrieveTextSMSTransmission(SMSAgent correspondent, boolean sent, int payloadType, int payloadHash)
	{
		
		return null;
	}

	public HTTPTransmission retrieveHTTPTransmission(int payloadType, int payloadHash)
	{
		
		return null;
	}
	
}
