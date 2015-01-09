package uk.ac.ucl.excites.sapelli.receiver;

public class TextSMSBroadcastReceiver extends SMSBroadcastReceiver
{

	@Override
	public boolean isBinary()
	{
		return false;
	}

}
