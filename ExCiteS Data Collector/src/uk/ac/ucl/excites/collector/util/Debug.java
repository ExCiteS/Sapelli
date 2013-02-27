package uk.ac.ucl.excites.collector.util;

import android.util.Log;

/**
 * Class for centralising the logging and debugging
 * 
 * @author Michalis Vitos
 * 
 */
public class Debug
{
	private static final boolean DEBUG = true;
	private final static String TAG = "ExCiteS_Debug";

	public static void v(String tag, String msg)
	{
		if(DEBUG)
			Log.v(tag, msg);
	}

	public static void v(String msg)
	{
		v(TAG, msg);
	}

	public static void d(String tag, String msg)
	{
		if(DEBUG)
			Log.d(tag, msg);
	}

	public static void d(String msg)
	{
		d(TAG, msg);
	}

	public static void i(String tag, String msg)
	{
		if(DEBUG)
			Log.i(tag, msg);
	}

	public static void i(String msg)
	{
		i(TAG, msg);
	}

	public static void w(String tag, String msg)
	{
		if(DEBUG)
			Log.w(tag, msg);
	}

	public static void w(String msg)
	{
		Debug.w(TAG, msg);
	}

	public static void e(String tag, String msg)
	{
		if(DEBUG)
			Log.e(tag, msg);
	}

	public static void e(String msg)
	{
		e(TAG, msg);
	}

	/**
	 * Display Stack Trace
	 * 
	 * @param e
	 */
	public static void e(String msg, Exception e)
	{
		if(DEBUG)
		{
			Log.e(TAG, "//================================================================================");
			Log.e(TAG, msg, e);
			Log.e(TAG, "//================================================================================");
		}
	}
}
