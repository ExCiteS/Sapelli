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
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.CheckBox;
import android.widget.LinearLayout.LayoutParams;

/**
 * @author Julia, mstevens
 * 
 */
public class AndroidCheckBoxUI extends CheckBoxUI<View, CollectorView>
{

	static private final float NEGATIVE_TOP_MARGIN_DIP = -5.5f;
	static private final float NEGATIVE_BOTTOM_MARGIN_DIP = -4.0f;
	
	private CheckBox chbx;
	private int negativeTopMarginPx;
	private int negativeBottomMarginPx;
	
	public AndroidCheckBoxUI(CheckBoxField checkBox, CollectorController controller, CollectorView collectorView)
	{
		super(checkBox, controller, collectorView);
		negativeTopMarginPx = collectorView.convertDipToPx(NEGATIVE_TOP_MARGIN_DIP);
		negativeBottomMarginPx = collectorView.convertDipToPx(NEGATIVE_BOTTOM_MARGIN_DIP);
	}

	@Override
	protected boolean getValue()
	{
		return chbx.isChecked();
	}
	
	@Override
	protected CheckBox getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if(chbx == null)
		{
			chbx = new CustomCheckBox(collectorUI.getContext(), onPage);
			newRecord = true; // force update of new view
		}
		
		// Update checkbox state:
		if(newRecord)
		{
			Boolean storedVal = field.getColumn().retrieveValue(record);
			chbx.setChecked(storedVal != null ? storedVal : field.getInitialValue()); 
		}
		chbx.setEnabled(enabled); // also sets up event listeners!
		
		// Return view:
		return chbx;
	}
	
	/**
	 * @author mstevens
	 */
	private class CustomCheckBox extends CheckBox implements OnClickListener
	{
		
		private boolean onPage;

		public CustomCheckBox(Context context, boolean onPage)
		{
			super(context);
			this.onPage = onPage;
			
			// Caption:
			setText(field.getCaption());
			
			// LayoutParams:
			LayoutParams lp = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
			lp.setMargins(0, negativeTopMarginPx, 0, negativeBottomMarginPx); // otherwise checkbox has too much margin on top & bottom (at least on Nexus4) // TODO test on XCover1 & 2)
			setLayoutParams(lp);
		}
		
		@Override
		public void onClick(View v)
		{
			collectorUI.revokeFocus(); // make other fields lose focus (notably to trigger validation on text fields)
		}
		
		@Override
		public void setEnabled(boolean enabled)
		{
			super.setEnabled(enabled);
			setOnClickListener(onPage && enabled ? this : null);
		}
		
	}
	
}
