package uk.ac.ucl.excites.sapelli.util;

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
			Log.v(tag, getPrintableName() + msg);
	}

	public static void v(String msg)
	{
		if(DEBUG)
			Log.v(TAG, getPrintableName() + msg);
	}

	public static void d(String tag, String msg)
	{
		if(DEBUG)
			Log.d(tag, msg);
	}

	public static void d(String msg)
	{
		if(DEBUG)
			Log.d(TAG, getPrintableName() + msg);
	}

	public static void d()
	{
		if(DEBUG)
			Log.d(TAG, getPrintableName());
	}

	public static void i(String tag, String msg)
	{
		if(DEBUG)
			Log.i(tag, getPrintableName() + msg);
	}

	public static void i(String msg)
	{
		if(DEBUG)
			Log.i(TAG, getPrintableName() + msg);
	}

	public static void w(String tag, String msg)
	{
		if(DEBUG)
			Log.w(tag, getPrintableName() + msg);
	}

	public static void w(String msg)
	{
		if(DEBUG)
			Log.w(TAG, getPrintableName() + msg);
	}

	public static void e(String tag, String msg)
	{
		if(DEBUG)
			Log.e(tag, getPrintableName() + msg);
	}

	public static void e(String msg)
	{
		if(DEBUG)
			Log.e(TAG, getPrintableName() + msg);
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
			Log.e(TAG, getPrintableName() + msg, e);
			Log.e(TAG, "//================================================================================");
		}
	}

	/**
	 * Display Stack Trace
	 * 
	 * @param e
	 */
	public static void e(Exception e)
	{
		if(DEBUG)
		{
			Log.e(TAG, "//================================================================================");
			Log.e(TAG, getPrintableName(), e);
			Log.e(TAG, "//================================================================================");
		}
	}

	public static String getPrintableName()
	{
		// Take the stack of the current thread
		final StackTraceElement stackTraceElement = Thread.currentThread().getStackTrace()[4];

		// Find the class name
		String packageName = stackTraceElement.getClassName().toString();
		String className = packageName.substring(packageName.lastIndexOf('.') + 1);

		String info = "[";
		info += stackTraceElement.getLineNumber() + " ";
		info += className + ":";
		info += stackTraceElement.getMethodName().toString();
		info += "] ";

		return info;
	}
}
