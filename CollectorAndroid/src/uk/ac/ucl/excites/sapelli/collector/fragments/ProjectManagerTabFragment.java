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
	
	private boolean skipRefreshOnNextResume = false;

	/**
	 * @param skipRefreshOnNextResume the skipRefreshOnNextResume to set
	 */
	public void setSkipRefreshOnNextResume(boolean skipRefreshOnNextResume)
	{
		this.skipRefreshOnNextResume = skipRefreshOnNextResume;
	}

	public abstract String getTabTitle(Context context);
	
	protected Project getProject(boolean errorIfNull)
	{
		ProjectManagerActivity activity = getOwner();
		return activity != null ? activity.getCurrentProject(errorIfNull) : null;
	}
	
	/* (non-Javadoc)
	 * @see android.support.v4.app.Fragment#onResume()
	 */
	@Override
	public void onResume()
	{
		super.onResume();
		
		if(!skipRefreshOnNextResume)
			refresh();
		else
			skipRefreshOnNextResume = false; // next time we will refresh
	}
	
	public final void refresh()
	{
		if(getOwner() != null && isUIReady())
			refresh(getProject(false));
	}
	
	/**
	 * @param project the currently active project (shouldn't be null but it is best to check)
	 */
	protected abstract void refresh(Project project);
	
}
