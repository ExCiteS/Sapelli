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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

/**
 * Android version of ButtonUI
 * 
 * @author mstevens
 */
public class AndroidButtonUI extends ButtonUI<View, CollectorView> implements OnClickListener
{
	// private CollectorController controller;
	private Button button;
	
	public AndroidButtonUI(ButtonField buttonField, CollectorController controller, CollectorView collectorView)
	{
		super(buttonField, controller, collectorView);
	}

	@Override
	public void onClick(View v)
	{
		controller.addLogLine("CLICK_BUTTON", this.field.id, this.field.getCaption());

		buttonPressed();
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if(button == null)
		{
			button = new Button(collectorUI.getContext());
			button.setText(field.getCaption());
			button.setOnClickListener(this);
		}
		
		// Update:
		button.setEnabled(enabled);
		
		return button;
	}

}
