package uk.ac.ucl.excites.sapelli.transmission.control;

import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;
import uk.ac.ucl.excites.sapelli.transmission.sender.gsm.SMSSender;
import android.content.Context;

public class AndroidTransmissionController extends TransmissionController
{

	private Context context;
	private SMSSender smsSender;
	
	public AndroidTransmissionController(TransmissionClient transmissionClient, TransmissionStore transmissionStore, Context context)
	{
		super(transmissionClient, transmissionStore);
		this.context = context;
	}

	@Override
	public SMSClient getSMSService()
	{
		if (smsSender == null)
			smsSender = new SMSSender(context);
		return smsSender;
	}

	@Override
	public HTTPClient getHTTPClient()
	{
		// TODO Auto-generated method stub
		return null;
	}

}
