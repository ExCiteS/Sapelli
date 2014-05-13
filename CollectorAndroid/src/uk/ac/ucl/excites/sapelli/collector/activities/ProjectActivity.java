/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.activities;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import android.os.Bundle;

/**
 * Abstract super class for {@link CollectorActivity} & {@link ExportActivity}, holding their shared data members and behaviour.
 * Provides methods to load a Project specified by an Intent parameter or project hash value.
 * 
 * @author mstevens
 */
public abstract class ProjectActivity extends BaseActivity implements StoreClient
{

	// INTENT PARAMETER:
	public static final String INTENT_PARAM_PROJECT_HASH = "Project_Hash"; // used on "direct" intents (coming from ProjectManagerActivity) & shortcut intents
	
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
			projectStore = app.getProjectStore(this);
			recordStore = app.getRecordStore(this);
		}
		catch(Exception e)
		{
			showErrorDialog("Could not open Project- or RecordStore, due to: " + ExceptionHelpers.getMessageAndCause(e), true);
			return;
		}
	}
	
	protected final void loadProject(boolean mandatory)
	{
		Bundle bundle = getIntent().getExtras();
		// Get project hash from extras (the key is the same for both call-by-intent & call-by-shortcut scenarios):
		if(bundle != null && bundle.containsKey(INTENT_PARAM_PROJECT_HASH))
			loadProject(bundle.getLong(INTENT_PARAM_PROJECT_HASH), mandatory);
		else if(mandatory)
			// show error (activity will be exited after used clicks OK in the dialog):
			showErrorDialog("Activity started without '" + INTENT_PARAM_PROJECT_HASH + "' intent parameter, don't know which project to load!", true);
	}
	
	protected final void loadProject(long projectHash, boolean mandatory)
	{
		this.project = projectStore.retrieveProject(projectHash);
		if(project == null && mandatory)
		{
			// show error (activity will be exited after used clicks OK in the dialog):
			showErrorDialog("Could not find project with hash: " + projectHash, true);
			return;
		}
		postLoadInitialisation();
	}
	
	/**
	 * Called after successful project loading
	 */
	protected void postLoadInitialisation()
	{
		// does nothing by default
	}
	
	@Override
	protected void onDestroy()
	{
		// Signal that the activity no longer needs the Store objects:
		app.discardStoreUsage(projectStore, this);
		app.discardStoreUsage(recordStore, this);
		// super:
		super.onDestroy();
	}

}
