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

package uk.ac.ucl.excites.sapelli.collector.activities;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import android.os.Bundle;

/**
 * Abstract super class for {@link CollectorActivity} & {@link ExportActivity}, holding their shared data members and behaviour.
 * Provides methods to load a Project specified by an Intent parameter or project hash value.
 * 
 * @author mstevens
 */
public abstract class ProjectActivity extends BaseActivity implements StoreHandle.StoreUser
{

	// INTENT PARAMETERS (used on "direct" intents; i.e. when coming from ProjectManagerActivity & in case of shortcut intents):
	public static final String INTENT_PARAM_PROJECT_ID = "Project_Id";
	public static final String INTENT_PARAM_PROJECT_FINGERPRINT = "Project_FingerPrint";
	public static final String INTENT_OPTIONAL_PARAM_SHORTCUT_NAME = "Project_Signature"; // optional: only present (i.e. non-null) when CollectorActivity is started through a shortcut
	
	protected ProjectStore projectStore;
	protected RecordStore recordStore;
	protected Project project;
	
	/**
	 * Note: loadProject() is *not* called here, subclasses need to handle its calling.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity#onCreate(android.os.Bundle)
	 */
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState); // sets the app member
	
		// Get project store:
		try
		{
			CollectorApp app = getCollectorApp();
			projectStore = app.collectorClient.projectStoreHandle.getStore(this);
			recordStore = app.collectorClient.recordStoreHandle.getStore(this);
		}
		catch(Exception e)
		{
			showErrorDialog("Could not open Project- or RecordStore, due to: " + ExceptionHelpers.getMessageAndCause(e), true);
			return;
		}
	}
	
	/**
	 * Loads the projects specified in the intent
	 * 
	 * @param mandatory
	 * @throws IllegalArgumentException when the specified project could not be loaded (but it was mandatory) 
	 * @throws IllegalStateException when no project was specified (but loading one was mandatory)
	 */
	protected void loadProject(boolean mandatory) throws IllegalArgumentException, IllegalStateException
	{
		Bundle bundle = getIntent().getExtras();
		// Get project id & fingerprint from extras (the key is the same for both call-by-intent & call-by-shortcut scenarios):
		if(bundle != null && bundle.containsKey(INTENT_PARAM_PROJECT_ID) && bundle.containsKey(INTENT_PARAM_PROJECT_FINGERPRINT))
		{
			this.project = projectStore.retrieveProject(bundle.getInt(INTENT_PARAM_PROJECT_ID), bundle.getInt(INTENT_PARAM_PROJECT_FINGERPRINT));
			if(project == null)
			{
				boolean fromShortcut = bundle.containsKey(INTENT_OPTIONAL_PARAM_SHORTCUT_NAME); 
				if(fromShortcut) // if we came here via a shortcut it should be removed so this doesn't happen again!
					ProjectRunHelpers.removeShortcut(this, bundle.getString(INTENT_OPTIONAL_PARAM_SHORTCUT_NAME), getIntent());
				if(mandatory) // inform caller:
					throw new IllegalArgumentException(	getString(	R.string.projectLoadFailure,
																	(fromShortcut ? bundle.getString(INTENT_OPTIONAL_PARAM_SHORTCUT_NAME) + " " : ""),
																	bundle.getInt(INTENT_PARAM_PROJECT_ID),
																	bundle.getInt(INTENT_PARAM_PROJECT_FINGERPRINT)) +
														(fromShortcut ? " " + getString(R.string.shortcutRemoved) : ""));
			}
		}
		else if(mandatory) // inform caller:
			throw new IllegalStateException(getString(R.string.noProjectSpecified)); // this should never happen
	}
	
	@Override
	protected void onDestroy()
	{
		// Signal that the activity no longer needs the Store objects:
		CollectorApp app = getCollectorApp();
		app.collectorClient.projectStoreHandle.doneUsing(this);
		app.collectorClient.recordStoreHandle.doneUsing(this);
		// super:
		super.onDestroy();
	}

}
