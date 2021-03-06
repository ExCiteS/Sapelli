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

package uk.ac.ucl.excites.sapelli.collector.fragments.tabs;

import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ExportFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;

/**
 * 
 * @author mstevens
 */
public class ExportTabFragment extends ProjectManagerTabFragment implements OnClickListener
{
	
	private ExportFragment exportFragment;
	
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.tab_export;
	}

	@Override
	protected void setupUI(View rootLayout)
	{
		// Export button:
		rootLayout.findViewById(R.id.btnExportForProject).setOnClickListener(this);
		
		// Export fragment:
		exportFragment = addChild(R.id.frgExportOnTabContainer, new ExportFragment());
	}

	@Override
	protected void refresh(Project project)
	{
		if(project != null)
			exportFragment.setProjectToExport(project);
	}

	@Override
	public void onClick(View v)
	{
		if(v.getId() == R.id.btnExportForProject)
			exportFragment.runExport();
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.export);
	}

}
