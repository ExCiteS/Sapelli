/**
 * Sapelli data collection platform: http://sapelli.org
 *
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.shared.util.android;

import android.util.Log;

import org.piwik.sdk.TrackHelper;
import org.piwik.sdk.Tracker;

import java.io.IOException;

import timber.log.Timber;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;

/**
 * Android-specific subclass of Logger which mirrors log writes to Android Logcat while writing to file and handles Analytics..
 *
 * @author benelliott, mstevens, Michalis Vitos
 */
public class AndroidLogger extends Logger
{

	private static String TAG = "SapelliLogger";

	// Statics
	private Tracker tracker;

	public AndroidLogger(String folderPath, String baseFileName, boolean printToOutputStream) throws IOException
	{
		super(folderPath, baseFileName, printToOutputStream);
	}

	public AndroidLogger(String folderPath, String baseFileName, boolean printToOutputStream, Tracker tracker) throws IOException
	{
		super(folderPath, baseFileName, printToOutputStream);
		this.tracker = tracker;
	}

	public AndroidLogger(String folderPath, String baseFileName, boolean timestampFilename, boolean printToOutputStream) throws IOException
	{
		super(folderPath, baseFileName, timestampFilename, printToOutputStream);
	}
	
	@Override
	protected void printToOutputStream(String line)
	{
		if(printToOutputStream)
			Log.i(TAG, line);
	}

	@Override
	public void logAnalytics(String... fields)
	{
		super.logAnalytics(fields);

		// Return if there are no fields
		if(fields == null || tracker == null)
			return;

		try
		{
			// Get category and action
			String category = "";
			String action = "";

			if(fields.length > 1)
				category = fields[0];

			if(fields.length >= 2)
				for(int i = 1; i < fields.length; i++)
					action += fields[i];

			// Category and Action cannot be empty
			if(category.isEmpty() || action.isEmpty())
				return;

			// Create label
			String label = category + ": " + action;

			// Track event
			TrackHelper.track()
			  .event(category, action)
			  .name(label)
			  .with(tracker);
		}
		catch(Exception e)
		{
			Timber.e(e, "Analytics error");
		}
	}
}
