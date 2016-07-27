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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.AndroidCollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;
import android.widget.TextView;


/**
 * @author mstevens
 *
 */
public class AndroidLabelUI extends LabelUI<View, CollectorView>
{
	
	private TextView label;
	
	public AndroidLabelUI(LabelField labelField, AndroidCollectorController controller, CollectorView collectorView)
	{
		super(labelField, controller, collectorView);
	}

	@Override
	protected TextView getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if(label == null)
		{
			label = new TextView(collectorUI.getContext());
			label.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
			
			// Alignment:
			label.setGravity(field.isCentered() ? Gravity.CENTER_HORIZONTAL : label.getGravity()); //TODO right alignment?
			
			// Font size:
			label.setText(field.getCaption());
			label.setTextSize(TypedValue.COMPLEX_UNIT_PX, label.getTextSize() * field.getTextSizeScale());
			
			//TODO bold? italic?
			
			// Set description for accessibility support:
			label.setContentDescription(field.description.getText());
		}
		return label;
	}

}
