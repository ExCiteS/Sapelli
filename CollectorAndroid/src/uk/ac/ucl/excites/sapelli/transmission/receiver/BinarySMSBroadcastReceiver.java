package uk.ac.ucl.excites.sapelli.transmission.receiver;

public class BinarySMSBroadcastReceiver extends SMSBroadcastReceiver
{

	@Override
	public boolean isBinary()
	{
		return true;
	}

}
