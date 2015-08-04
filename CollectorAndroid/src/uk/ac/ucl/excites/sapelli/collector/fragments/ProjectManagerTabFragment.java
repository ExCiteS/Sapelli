/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.fragments;

import android.content.Context;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.model.Project;

/**
 * @author mstevens
 *
 */
public abstract class ProjectManagerTabFragment extends ProjectManagerFragment
{

	public abstract String getTabTitle(Context context);
	
	protected Project getProject()
	{
		ProjectManagerActivity activity = getOwner();
		return activity != null ? activity.getCurrentProject(false) : null;
	}
	
}
