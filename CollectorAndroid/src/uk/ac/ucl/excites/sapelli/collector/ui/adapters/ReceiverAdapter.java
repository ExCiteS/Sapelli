/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.collector.ui.adapters;

import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverDrawableProvider;
import uk.ac.ucl.excites.sapelli.shared.util.android.AdvancedSpinnerAdapter;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;

/**
 * @author mstevens
 */
public class ReceiverAdapter extends AdvancedSpinnerAdapter<Correspondent>
{
	
	public ReceiverAdapter(ProjectManagerActivity owner, List<Correspondent> receivers, boolean showPleaseSelect)
	{
		super(	owner,
				R.layout.receiver_item,
				R.layout.receiver_dropdown_item,
				AdvancedSpinnerAdapter.TEXTVIEW_RESOURCE_ID_WHOLE_LAYOUT,
				showPleaseSelect ? owner.getString(R.string.lstPleaseSelect) : null,
				null,
				receivers);
	}
	
	@Override
	protected Integer getItemDrawableResourceId(int position, Correspondent receiver)
	{
		if(receiver == null)
			return null;
		//else:
		ReceiverDrawableProvider provider = new ReceiverDrawableProvider();
		receiver.handle(provider);
		return provider.drawableResourceId;
	}
	
}