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

package uk.ac.ucl.excites.sapelli.collector.db;

import java.util.Map;

import uk.ac.ucl.excites.sapelli.shared.util.android.Debug;
import android.content.Context;
import android.content.SharedPreferences;

/**
 * Sapelli Collector preferences using Android SharedPreferences
 * 
 * @author Michalis Vitos
 */
public class CollectorPreferences
{
	// Statics----------------------------------------------
	private static final String PREFERENCES_NAME = "COLLECTOR_PREFERANCES";
	private static final String PREF_SAPELLI_FOLDER = "SAPELLI_FOLDER";
	private static final String PREF_FIRST_INSTALLATION = "FIRST_INSTALLATION";

	// Dynamics---------------------------------------------
	private SharedPreferences preferences;

	public CollectorPreferences(Context context)
	{
		this.preferences = context.getSharedPreferences(PREFERENCES_NAME, Context.MODE_PRIVATE);
	}
	
	/**
	 * Store the Sapelli Folder path in the preferences
	 * 
	 * @param folderPath
	 */
	public void setSapelliFolder(String folderPath)
	{
		preferences.edit().putString(PREF_SAPELLI_FOLDER, folderPath).commit();
	}

	/**
	 * Retrieve the Sapelli Folder path from the preferences
	 * 
	 * @return
	 */
	public String getSapelliFolderPath()
	{
		return preferences.getString(PREF_SAPELLI_FOLDER, null);
	}

	/**
	 * Clear the Sapelli Folder path from the preferences
	 */
	public void clearSapelliFolder()
	{
		preferences.edit().remove(PREF_SAPELLI_FOLDER).commit();
	}

	/**
	 * Set the first installation
	 * 
	 * @param firstInstallation
	 */
	public void setFirstInstallation(boolean firstInstallation)
	{
		preferences.edit().putBoolean(PREF_FIRST_INSTALLATION, firstInstallation).commit();
	}

	/**
	 * Check if the app is installed for the first time
	 * 
	 * @return true if is first installation, false otherwise
	 */
	public boolean isFirstInstallation()
	{
		return preferences.getBoolean(PREF_FIRST_INSTALLATION, true);
	}

	public void printAll()
	{
		Map<String, ?> keys = preferences.getAll();

		Debug.d("Collector preferences: ");

		for(Map.Entry<String, ?> entry : keys.entrySet())
			Debug.d(entry.getKey() + ": " + entry.getValue().toString());
	}

}
