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

import java.io.IOException;

import android.util.Log;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;

/**
 * Android-specific subclass of Logger which mirrors log writes to Android Logcat while writing to file.
 * 
 * @author benelliott, mstevens
 */
public class AndroidLogger extends Logger
{

	private static String TAG = "SapelliLogger";
	
	public AndroidLogger(String folderPath, String baseFileName, boolean printToOutputStream) throws IOException
	{
		super(folderPath, baseFileName, printToOutputStream);
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

}
