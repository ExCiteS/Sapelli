package uk.ac.ucl.excites.sapelli.transmission.control;

import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;

public class AndroidTransmissionController extends TransmissionController
{

	public AndroidTransmissionController(TransmissionClient transmissionClient, TransmissionStore transmissionStore)
	{
		super(transmissionClient, transmissionStore);
	}

	@Override
	public SMSClient getSMSService()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public HTTPClient getHTTPClient()
	{
		// TODO Auto-generated method stub
		return null;
	}

}
