package uk.ac.ucl.excites.sapelli.receiver;

public class BinarySMSBroadcastReceiver extends SMSBroadcastReceiver
{

	@Override
	public boolean isBinary()
	{
		return true;
	}

}
