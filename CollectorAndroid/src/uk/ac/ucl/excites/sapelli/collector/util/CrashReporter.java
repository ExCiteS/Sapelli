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
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.Thread.UncaughtExceptionHandler;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.BuildInfo;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
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
	private FileStorageProvider pathProvider;
	private String namePrefix;

	public CrashReporter(FileStorageProvider pathProvider, String namePrefix)
	{
		this.pathProvider = pathProvider;
		this.namePrefix = namePrefix;

		this.defaultUEH = Thread.getDefaultUncaughtExceptionHandler();
	}

	public void uncaughtException(Thread t, Throwable e)
	{
		StringBuilder bldr = new StringBuilder();
		
		// Timestamp:
		DateTime ts = DateTime.now();
		bldr.append("Time of crash: " + TimeUtils.ISOWithMSFormatter.withZone(ts.getZone()).print(ts) + "\n\n");
		
		// The actual stacktrace:
		bldr.append("Stacktrace:\n");
		Writer result = new StringWriter();
		PrintWriter printWriter = new PrintWriter(result);
		e.printStackTrace(printWriter);
		bldr.append(result.toString());
		printWriter.close();
		bldr.append("\n");
		
		// Add Sapelli Collector build information:
		BuildInfo buildInfo = BuildInfo.GetInstance();
		String lastProject = System.getProperty(CollectorApp.PROPERTY_LAST_PROJECT);
		if(buildInfo != null || lastProject != null)
		{
			bldr.append("Application info:\n");
			if(buildInfo != null)
			{
				bldr.append("\t" + buildInfo.getVersionInfo() + ".\n");
				bldr.append("\t" + buildInfo.getBuildInfo() + ".\n");
			}
			if(lastProject != null)
				bldr.append("\t(last) running project: " + lastProject + "\n");
			bldr.append("\n");
		}
		
		// Add device info:
		bldr.append("Device hardware info:\n\t");
		bldr.append(DeviceID.getHardwareInfo("\n\t") + "\n\n");
		bldr.append("Device software info:\n\t");
		bldr.append(DeviceID.getSoftwareInfo("\n\t") + "\n\n");
		
		// Add device ID:
		DeviceID deviceID = DeviceID.GetInstanceOrNull();
		if(deviceID != null)
		{
			bldr.append("Sapelli DeviceID:\n");
			bldr.append("\tCRC32: " + deviceID.getIDAsCRC32Hash() + "\n");
			bldr.append("\tMD5: " + deviceID.getIDAsMD5Hash() + "\n");
			bldr.append("\tRaw: " + deviceID.getRawID() + "\n\n");
		}
		
		// Write to file:
		writeToFile(bldr.toString(), namePrefix + "_" + TimeUtils.getTimestampForFileName() + ".stacktrace");

		// Pass on the exception:
		defaultUEH.uncaughtException(t, e);
	}

	private void writeToFile(String stacktrace, String filename)
	{
		BufferedWriter bos = null;
		try
		{
			bos = new BufferedWriter(new FileWriter(pathProvider.getDumpFolder(true).getAbsolutePath() + File.separator + filename));
			bos.write(stacktrace);
			bos.flush();
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		finally
		{
			if(bos != null)
				try
				{
					bos.close();
				}
				catch(IOException ignore) {}
		}
	}
	
}