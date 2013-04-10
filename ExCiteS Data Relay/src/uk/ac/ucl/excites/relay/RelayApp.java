package uk.ac.ucl.excites.relay;

import java.io.File;

import uk.ac.ucl.excites.relay.util.CrashReporter;
import android.app.Application;
import android.os.Environment;

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

	private static final String CRASH_FOLDER = "ExCiteS" + File.separator + "crash";

	@Override
	public void onCreate()
	{
		super.onCreate();
		
		// Set up a CrashReporter to the ExCiteS/crash Folder
		final String localPath = Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator + CRASH_FOLDER;
		Thread.setDefaultUncaughtExceptionHandler(new CrashReporter(localPath, getResources().getString(R.string.app_name)));
	}
	
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
