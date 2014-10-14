/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.util;

import uk.ac.ucl.excites.sapelli.collector.BuildConfig;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

/**
 * Class for centralising the logging and debugging
 * 
 * @author Michalis Vitos
 * 
 */
public class Debug
{
	private static final boolean DEBUG = BuildConfig.DEBUG;
	private final static String TAG = "Sapelli";

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

	public static String printIntentExtras(Intent intent)
	{
		String txt = "";
		final Bundle extras = intent.getExtras();

		for(String key : extras.keySet())
		{
			Object value = extras.get(key);
			txt += "\n" + String.format("%s %s (%s)", key + ":", value.toString(), value.getClass().getName()) + ",";
		}

		return txt;
	}
}
