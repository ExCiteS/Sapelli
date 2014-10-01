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

package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.Thread.UncaughtExceptionHandler;

import uk.ac.ucl.excites.sapelli.collector.BuildInfo;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;

/**
 * Simple Class to Log App Crashes to a file<br>
 * in order to use call: <br>
 * <code>Thread.setDefaultUncaughtExceptionHandler(new CrashReporter(localPath, getResources().getString(R.string.app_name)))</code>
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
public class CrashReporter implements UncaughtExceptionHandler
{
	private UncaughtExceptionHandler defaultUEH;
	private String localPath;
	private String namePrefix;
	private DeviceID deviceID;

	public CrashReporter(String localPath, String namePrefix)
	{
		this.localPath = localPath;
		this.namePrefix = namePrefix;

		// Create the folder if does not exist
		FileHelpers.createFolder(localPath);
		this.defaultUEH = Thread.getDefaultUncaughtExceptionHandler();
	}

	public void uncaughtException(Thread t, Throwable e)
	{
		final Writer result = new StringWriter();
		final PrintWriter printWriter = new PrintWriter(result);
		e.printStackTrace(printWriter);
		String stacktrace = result.toString();
		printWriter.close();

		// Add build information to the stacktrace
		if(deviceID != null)
		{
			stacktrace = "Sapelli DeviceID (CRC32): " + deviceID.getIDAsCRC32Hash() 
					+ "\n" 
					+ "Sapelli DeviceID (MD5): " + deviceID.getIDAsMD5Hash().toString() 
					+ "\n\n" 
					+ stacktrace;
		}
		stacktrace = BuildInfo.getAllInfo() + "\n\n" + stacktrace;

		String filename = namePrefix + "_" + TimeUtils.getTimestampForFileName() + ".stacktrace";
		if(localPath != null)
			writeToFile(stacktrace, filename);

		defaultUEH.uncaughtException(t, e);
	}

	/**
	 * @param deviceID
	 *            the deviceID to set
	 */
	public void setDeviceID(DeviceID deviceID)
	{
		this.deviceID = deviceID;
	}

	private void writeToFile(String stacktrace, String filename)
	{
		try
		{
			BufferedWriter bos = new BufferedWriter(new FileWriter(localPath + "/" + filename));
			bos.write(stacktrace);
			bos.flush();
			bos.close();
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}
}