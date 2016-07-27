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

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.view.View;
import android.widget.TextView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;

/**
 * 
 * 
 * @author Julia, mstevens
 */
public class DetailsTabFragment extends ProjectManagerTabFragment
{
	
	static private int[] labelIDs =
	{
		R.id.lblProjectName,
		R.id.lblProjectVariant,
		R.id.lblProjectVersion,
		R.id.lblProjectID,
		R.id.lblProjectFingerPrint,
		R.id.lblProjectModelID,
		R.id.lblProjectNumberOfForms
	};

	private final List<TextView> lblTextViews = new ArrayList<TextView>(labelIDs.length);

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
		for(int labelID : labelIDs)
			lblTextViews.add((TextView) rootLayout.findViewById(labelID));
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_details);
	}

	@Override
	protected void refresh(Project project)
	{
		if(project != null)
			for(TextView lblTextView : lblTextViews)
				switch(lblTextView.getId())
				{
					case R.id.lblProjectName :
						lblTextView.setText(project.getName());
						break;					
					case R.id.lblProjectVariant :
						lblTextView.setText(project.getVariant() != null ? project.getVariant() : "");
						break;
					case R.id.lblProjectVersion :
						lblTextView.setText(project.getVersion());
						break;
					case R.id.lblProjectID :
						lblTextView.setText(Integer.toString(project.getID()));
						break;
					case R.id.lblProjectFingerPrint :
						lblTextView.setText(Integer.toString(project.getFingerPrint()));
						break;
					case R.id.lblProjectModelID :
						lblTextView.setText(Long.toString(project.getModel().id));
						break;
					case R.id.lblProjectNumberOfForms :
						lblTextView.setText(Integer.toString(project.getNumberOfForms()));
						break;
				}
	}

}
