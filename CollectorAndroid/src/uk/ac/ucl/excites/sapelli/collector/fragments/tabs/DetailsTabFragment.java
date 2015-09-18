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

package uk.ac.ucl.excites.sapelli.collector.fragments.tabs;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import android.content.Context;
import android.view.View;
import android.widget.TextView;

/**
 * 
 * 
 * @author Julia, mstevens
 */
public class DetailsTabFragment extends ProjectManagerTabFragment
{

	public static DetailsTabFragment newInstance()
	{
		DetailsTabFragment f = new DetailsTabFragment();
		return f;
	}

	@Override
	protected Integer getLayoutID()
	{
		return R.layout.tab_details;
	}

	@Override
	protected void setupUI(View rootLayout)
	{
		Project project = getOwner().getCurrentProject(false);
		if(project != null)
		{
			((TextView) rootLayout.findViewById(R.id.lblProjectName)).setText(project.getName());
			((TextView) rootLayout.findViewById(R.id.lblProjectID)).setText(Integer.toString(project.getID()));
			((TextView) rootLayout.findViewById(R.id.lblProjectVariant)).setText(project.getVariant() != null ? project.getVariant() : "");
			((TextView) rootLayout.findViewById(R.id.lblProjectVersion)).setText(project.getVersion());
			((TextView) rootLayout.findViewById(R.id.lblProjectFingerPrint)).setText(Integer.toString(project.getFingerPrint()));
			((TextView) rootLayout.findViewById(R.id.lblProjectNumberOfForms)).setText(Integer.toString(project.getNumberOfForms()));
			((TextView) rootLayout.findViewById(R.id.lblProjectModelID)).setText(Long.toString(project.getModel().id));
		}
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_details);
	}

}
