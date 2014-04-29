package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnFocusChangeListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.LinearLayout;
import android.widget.Spinner;
import android.widget.SpinnerAdapter;
import android.widget.TextView;

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
			if(!field.isNoColumn() && field.getColumn().isValueSet(record))
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
			// Add first spinner, if there is a value in the column the remaining spinner(s) will follow automatically by selection from the selectionStack:
			view.addNextList(field.getItemsRoot());
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
			
			if(!selectionStack.isEmpty())
				spinner.selectItem(selectionStack.pop());
			//	Select default if preSelect=true:
			else if(field.isPreSelect())
				spinner.setSelection(parentItem.getDefaultChildIndex());
			//else: first (dummy) item will be selected
			
			//	Enable/disable spinner (also sets focus event listener):
			spinner.setEnabled(isEnabled());
			
			//	Item selected event:
			spinner.setOnItemSelectedListener(this);
			
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
			MultiListAdapter adapter = spinner.getAdapter();
			
			revert(spinner);
			
			MultiListItem chosen = adapter.getItem(position);
			if(chosen != adapter.nonSelectableItem && chosen != adapter.nullItem && !chosen.isLeaf())
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
			MultiListAdapter adapter = getAdapter();
			MultiListItem selected = adapter.getItem(getSelectedItemPosition());
			if(selected != adapter.nonSelectableItem && selected != adapter.nullItem)
				return selected;
			else
				return null;
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
	 * Custom ArrayAdapter to allow simulation of the spinner being in an "unselected" state (not supported on Android).
	 * 
	 * @author mstevens
	 * 
	 * @see <a href="http://stackoverflow.com/questions/9863378">http://stackoverflow.com/questions/9863378</a>
	 */
	private class MultiListAdapter extends ArrayAdapter<MultiListItem>
	{
		
		private MultiListItem nonSelectableItem;
		private MultiListItem nullItem;
		
		public MultiListAdapter(Context context, MultiListItem parentItem)
		{
			super(context, android.R.layout.simple_spinner_item);
			this.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
			
			// Insert "Please select" item if preSelect=false:
			if(!parentItem.getField().isPreSelect())
			{
				nonSelectableItem = MultiListItem.GetDummyItem(field, PLEASE_SELECT);
				this.add(nonSelectableItem);
			}
			
			// Add real children:
			for(MultiListItem item : parentItem.getChildren())
				this.add(item); // Not using this.addAll(...) because it requires API level 11 (current minimum is 9)
			
			// If preSelect=true, but the field is optional...
			if(parentItem.getField().isPreSelect() && parentItem.getField().getOptional() == Optionalness.ALWAYS)
			{	// insert "null-selection" item such that "not answering" remains possible:
				nullItem = MultiListItem.GetDummyItem(field, UNDO_SELECTION);
				this.add(nullItem);
			}
		}
		
		@Override
		public View getView(int position, View convertView, ViewGroup parent)
		{
			TextView v = (TextView) super.getView(position, null, parent);
			if((nonSelectableItem != null && position == 0) || (nullItem != null && position == getCount() - 1))
				v.setGravity(Gravity.CENTER); // Centre the text on the nonSelectedDefault item and on the nullItem
			return v;
		}
		
		@Override
		public View getDropDownView(int position, View convertView, ViewGroup parent)
		{
			if(nonSelectableItem != null && position == 0)
			{	// Hide the nonSelectedDefault item:
				TextView dummyView = new TextView(getContext());
	            dummyView.setHeight(0);
	            //dummyView.setVisibility(View.GONE); //does not seem to make a difference
	            return dummyView;
			}
			else
				return super.getDropDownView(position, null, parent);
		}

	}

}
