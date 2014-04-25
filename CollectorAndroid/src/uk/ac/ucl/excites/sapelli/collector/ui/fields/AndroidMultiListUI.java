package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.FormMode;
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
	protected MultiListView getPlatformView(boolean onPage, Record record, boolean newRecord)
	{	
		if(view == null)
		{
			view = new MultiListView(onPage, collectorUI.getContext());
			newRecord = true; // force update of new view
		}
		
		// Update view:
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
		// Enable/disable:
		view.setEnabled(controller.getCurrentFormMode() != FormMode.EDIT || field.isEditable()); // disable when in edit mode and field is not editable, otherwise enable
		
		return view;
	}
	
	@Override
	protected MultiListItem getChosenItem()
	{
		if(view == null)
			return null;
		return view.getBottomSpinner().getSelectedItem(); // return selected item of bottom spinner (may be null and not necessarily a leaf)
	}
	
	private class MultiListView extends LinearLayout implements OnFocusChangeListener, OnItemSelectedListener
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
			final MultiListSpinner spinner = new MultiListSpinner(getContext());
			spinner.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
			
			//	Adapter:
			final MultiListAdapter adapter = new MultiListAdapter(getContext(), parentItem);
			spinner.setAdapter(adapter);
			
			if(!selectionStack.isEmpty())
				spinner.selectItem(selectionStack.pop());
			//	Select default if preSelect=true:
			else if(field.isPreSelect())
				spinner.setSelection(parentItem.getDefaultChildIndex());
			//else: first (dummy) item will be selected
			
			//	If on page: make other fields lose focus, make keyboard disappear, and simulate clicking with onFocusChange:
			spinner.setFocusable(onPage);
			spinner.setFocusableInTouchMode(onPage);
			spinner.setOnFocusChangeListener(onPage ? this : null);
			
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
		public void onFocusChange(View v, boolean hasFocus)
		{
			if(hasFocus)
			{
				// Hide keyboard if it is currently shown:
				collectorUI.hideKeyboard();
				
				// Simulate click:
				((MultiListSpinner) v).performClick();
				
				// Lose focus again:
				v.clearFocus();
			}
		}
		
		@Override
		public void onItemSelected(AdapterView<?> parent, View view, int position, long id)
		{
			clearPageInvalidMark(); // the user is currently interacting with the spinner(s), so don't annoy him/her with the red box
			
			MultiListSpinner spinner = (MultiListSpinner) parent; 
			MultiListAdapter adapter = spinner.getAdapter();
			
			revert(spinner);
			//requestPageRevalidation();
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
			// Apply to all spinners:
			for(int i = 1; i < getChildCount(); i+=2)
				getChildAt(i).setEnabled(enabled);
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
	
	/**
	 * @author mstevens
	 *
	 */
	private class MultiListSpinner extends Spinner
	{

		public MultiListSpinner(Context context)
		{
			super(context);
		}
		
		@Override
		public void setAdapter(SpinnerAdapter adapter)
		{
			if(adapter instanceof MultiListAdapter)
				setAdapter((MultiListAdapter) adapter);
			else
				throw new IllegalArgumentException("MultiListSpinner only takes a MultiListAdapter instance as its adapter.");
		}

		public void setAdapter(MultiListAdapter adapter)
		{
			super.setAdapter(adapter);
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
		
	}

}
