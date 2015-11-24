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

import java.util.Stack;

import android.content.Context;
import android.view.View;
import android.view.View.OnFocusChangeListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.LinearLayout;
import android.widget.Spinner;
import android.widget.SpinnerAdapter;
import android.widget.TextView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.shared.util.android.AdvancedSpinnerAdapter;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Android version of MultiListUI
 * 
 * @author mstevens
 */
public class AndroidMultiListUI extends MultiListUI<View, CollectorView>
{
	
	private MultiListView view;
	private Stack<MultiListItem> selectionStack;
	
	public AndroidMultiListUI(MultiListField listField, CollectorController controller, CollectorView collectorView)
	{
		super(listField, controller, collectorView);
		selectionStack = new Stack<MultiListItem>();
	}
	
	@Override
	protected MultiListView getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{	
		if(view == null)
		{
			view = new MultiListView(onPage, collectorUI.getContext());
			newRecord = true; // force update of new view
		}
		
		// Update view:
		//	Enable/disable:
		view.setEnabled(enabled); // also sets-up event listeners
		//	For new records:
		if(newRecord)
		{
			// Set the value that was stored (if there is one):
			if(!field.isNoColumn() && field.getColumn().isValuePresent(record))
			{
				// Get selected leaf:
				MultiListItem item = field.getItemForValue(field.getColumn().retrieveValue(record).intValue());
				
				// Build up path from selected leaf to root:
				selectionStack.clear(); // just in case
				while(!item.isRoot())
				{
					selectionStack.push(item);
					item = item.getParent();
				}
			}
			// Remove all spinners
			view.fullRevert();
			
			// Briefly disable automatic pop-up of next spinner:
			view.setAutoOpenToSelect(false);
			
			// Add first spinner, if there is a value in the column the remaining spinner(s) will follow automatically by selection from the selectionStack:
			view.addNextList(field.getItemsRoot());
			
			// Re-enable automatic pop-up:
			view.setAutoOpenToSelect(true);
		}
		
		return view;
	}
	
	@Override
	protected MultiListItem getChosenItem()
	{
		if(view == null)
			return null;
		return view.getBottomSpinner().getSelectedItem(); // return selected item of bottom spinner (may be null and not necessarily a leaf)
	}
	
	private class MultiListView extends LinearLayout implements OnItemSelectedListener
	{

		private boolean onPage;
		private boolean autoOpenToSelect = true;
		
		public MultiListView(boolean onPage, Context context)
		{
			super(context);
			this.onPage = onPage;
			setOrientation(LinearLayout.VERTICAL);
			setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
		}
		
		public void addNextList(MultiListItem parentItem)
		{			
			int level = getChildCount() / 2;
			
			// Label:
			TextView label = new TextView(getContext());
			label.setText(field.getCaption(level));
			label.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
			addView(label);
			
			// Spinner (combo box):
			final MultiListSpinner spinner = new MultiListSpinner(getContext(), parentItem, onPage);
			spinner.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
			spinner.setPrompt(field.getCaption(level));
			
			//	Item selected event:
			spinner.setOnItemSelectedListener(this);
			
			//	Item selection:
			//		Current value:
			if(!selectionStack.isEmpty())
				spinner.selectItem(selectionStack.pop());
			//		Select default if preSelect=true:
			else if(field.isPreSelect())
				spinner.setSelection(parentItem.getDefaultChildIndex());
			//		Simulate user click:
			else if(autoOpenToSelect)
				spinner.performClick();
			//else: first (dummy) item will be selected
			
			//	Enable/disable spinner (also sets focus event listener):
			spinner.setEnabled(isEnabled());
			
			//	Add the spinner:
			addView(spinner);
		}
		
		public void fullRevert()
		{
			revert(null);
		}
		
		public void revert(Spinner till)
		{
			while(getChildCount() > 0 && getChildAt(getChildCount() - 1) != till)
			{
				// Remove last spinner
				removeViewAt(getChildCount() - 1);
				// Remove its label:
				removeViewAt(getChildCount() - 1);
			}
		}
		
		@Override
		public void onItemSelected(AdapterView<?> parent, View view, int position, long id)
		{
			clearPageInvalidMark(); // the user is currently interacting with the spinner(s), so don't annoy him/her with the red box			

			MultiListSpinner spinner = (MultiListSpinner) parent;
			
			revert(spinner);
			
			MultiListItem chosen = spinner.getAdapter().getItem(position);
			if(chosen != null && !chosen.isLeaf())
				addNextList(chosen);
		}

		@Override
		public void onNothingSelected(AdapterView<?> parent)
		{
			revert((MultiListSpinner) parent);
		}
		
		public MultiListSpinner getBottomSpinner()
		{
			return ((MultiListSpinner) getChildAt(getChildCount() - 1));
		}
		
		@Override
		public void setEnabled(boolean enabled)
		{
			super.setEnabled(enabled);
			// Apply to all current spinners:
			for(int i = 1; i < getChildCount(); i+=2)
				getChildAt(i).setEnabled(enabled); // every second child is a spinner
		}

		/**
		 * @param autoOpenToSelect the autoOpenToSelect to set
		 */
		public void setAutoOpenToSelect(boolean autoOpenToSelect)
		{
			this.autoOpenToSelect = autoOpenToSelect;
		}

	}
	
	/**
	 * Custom Spinner class
	 * 
	 * @author mstevens
	 */
	private class MultiListSpinner extends Spinner implements OnFocusChangeListener
	{
		
		private boolean onPage;

		public MultiListSpinner(Context context, MultiListItem parentItem, boolean onPage)
		{
			super(context);
			this.onPage = onPage;
			// Set adapter:
			super.setAdapter(new MultiListAdapter(context, parentItem));
	}
		
		@Override
		public void setAdapter(SpinnerAdapter adapter)
		{
			throw new UnsupportedOperationException("Changing the adapter on MultiListSpinner is not allowed.");
		}

		@Override
		public MultiListAdapter getAdapter()
		{
			return (MultiListAdapter) super.getAdapter();
		}
		
		public void selectItem(MultiListItem item)
		{
			setSelection(getAdapter().getPosition(item));
		}
		
		@Override
		public MultiListItem getSelectedItem()
		{
			return (MultiListItem) super.getSelectedItem();
		}
		
		@Override
		public void setEnabled(boolean enabled)
		{
			super.setEnabled(enabled);
			//	If on page & enabled: make other fields lose focus and simulate clicking with onFocusChange:
			setFocusable(onPage && enabled);
			setFocusableInTouchMode(onPage && enabled);
			setOnFocusChangeListener(onPage && enabled ? this : null);
		}
		
		@Override
		public void onFocusChange(View v, boolean hasFocus)
		{
			if(hasFocus && isFieldShown() && isEnabled())
			{
				// Simulate click:
				((MultiListSpinner) v).performClick();
				
				// Lose focus again:
				v.clearFocus();
			}
		}
		
	}
	
	/**
	 * @see AdvancedSpinnerAdapter
	 * @author mstevens
	 */
	private class MultiListAdapter extends AdvancedSpinnerAdapter<MultiListItem>
	{
		
		public MultiListAdapter(Context context, MultiListItem parentItem)
		{
			super(	context,
					// If preSelect=false? insert "Please select" item:
					!parentItem.getField().isPreSelect() ? context.getString(R.string.lstPleaseSelect) : null,
					// If preSelect=true, but the field is optional, insert "null-selection" item such that "not answering" remains possible: 
					parentItem.getField().isPreSelect() && parentItem.getField().isOptional() ? context.getString(R.string.lstUndoSelection) : null,
					// The real children:
					parentItem.getChildren());
		}

	}

}

