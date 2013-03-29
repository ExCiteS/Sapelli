package uk.ac.ucl.excites.relay;

import android.app.Application;

/**
 * The Application Object used to keep some Global values
 * 
 * @author Michalis Vitos
 * 
 */
public class RelayApp extends Application
{
	private static long lastReceivedSMS;
	private static long lastSentSMS;

	/**
	 * @return the lastReceivedSMS
	 */
	public static long getLastReceivedSMS()
	{
		return lastReceivedSMS;
	}

	/**
	 * @param lastReceivedSMS
	 *            the lastReceivedSMS to set
	 */
	public static void setLastReceivedSMS(long lastReceivedSMS)
	{
		RelayApp.lastReceivedSMS = lastReceivedSMS;
	}

	/**
	 * @return the lastSentSMS
	 */
	public static long getLastSentSMS()
	{
		return lastSentSMS;
	}

	/**
	 * @param lastSentSMS
	 *            the lastSentSMS to set
	 */
	public static void setLastSentSMS(long lastSentSMS)
	{
		RelayApp.lastSentSMS = lastSentSMS;
	}
}
