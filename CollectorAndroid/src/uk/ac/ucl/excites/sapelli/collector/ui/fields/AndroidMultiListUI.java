package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
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
	
	private LinearLayout view;
	private CollectorRecord lastKnownRecord = null;
	private Stack<MultiListItem> selectionStack;
	
	public AndroidMultiListUI(MultiListField listField, CollectorController controller, CollectorView collectorView)
	{
		super(listField, controller, collectorView);
		selectionStack = new Stack<MultiListItem>();
	}
	
	@Override
	public LinearLayout getPlatformView(boolean onPage, CollectorRecord record)
	{	
		if(view == null)
		{
			view = new LinearLayout(collectorUI.getContext());
			view.setOrientation(LinearLayout.VERTICAL);
			view.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
		}
		
		// Update view:
		if(!field.isNoColumn() && field.getColumn().isValueSet(record))
		{	// Value set in column (probably we are editing):
			
			// Get selected leaf:
			MultiListItem item = field.getItemForValue(field.getColumn().retrieveValue(record).intValue());
			
			// Build up path from selected leaf to root:
			selectionStack.clear(); // just in case
			while(!item.isRoot())
			{
				selectionStack.push(item);
				item = item.getParent();
			}
			
			// Remove all spinners	
			fullRevert();
			// Add first spinner, the rest will following automatically by selection from the selectionStack:
			addNextList(field.getItemsRoot());
		}
		else if(record != lastKnownRecord)
		{	// New record:
			fullRevert();
			// (Re)add labels/spinners & perform pre-selection if needed:
			addNextList(field.getItemsRoot());
		}
		
		// Remember record:
		lastKnownRecord = record;
		
		return view;
	}
	
	@Override
	protected MultiListItem getChosenItem()
	{
		return ((MultiListSpinner) view.getChildAt(view.getChildCount() - 1)).getSelectedItem(); // return selected item of bottom spinner (may be null and not necessarily a leaf)
	}
	
	private MultiListSpinner addNextList(MultiListItem parentItem)
	{
		// Null check:
		if(view == null)
			return null;
		
		int level = view.getChildCount() / 2;
		
		// Label:
		TextView label = new TextView(view.getContext());
		label.setText(field.getLabel(level));
		label.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
		view.addView(label);
		
		// Combo box:
		final MultiListSpinner spinner = new MultiListSpinner(view.getContext());
		spinner.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
		
		//	Adapter:
		final MultiListAdapter adapter = new MultiListAdapter(view.getContext(), parentItem);
		spinner.setAdapter(adapter);
		
		if(!selectionStack.isEmpty())
			spinner.selectItem(selectionStack.pop());
		//	Select default if preSelect=true:
		else if(field.isPreSelect())
			spinner.setSelection(parentItem.getDefaultChildIndex());
		//else: first (dummy) item will be selected
		
		// Make other fields lose focus, make keyboard disappear, and simulate clicking with onFocusChange:
		spinner.setFocusable(true);
		spinner.setFocusableInTouchMode(true);
		spinner.setOnFocusChangeListener(new OnFocusChangeListener()
		{
			@Override
			public void onFocusChange(View v, boolean hasFocus)
			{
				if(hasFocus)
				{
					// Hide keyboard if it is currently shown:
					collectorUI.hideKeyboard();
					
					// Simulate click:
					((Spinner) v).performClick();
					
					// Lose focus again:
					v.clearFocus();
				}
			}
		});
		
		// 	Item selected event:
		spinner.setOnItemSelectedListener(new OnItemSelectedListener()
		{

			@Override
			public void onItemSelected(AdapterView<?> parent, View view, int position, long id)
			{
				revert(spinner);
				//requestPageRevalidation();
				MultiListItem chosen = adapter.getItem(position);
				if(chosen != adapter.nonSelectableItem && chosen != adapter.nullItem && !chosen.isLeaf())
					addNextList(chosen);
			}

			@Override
			public void onNothingSelected(AdapterView<?> parent)
			{
				revert(spinner);
			}
			
		});
	
		view.addView(spinner);
		
		return spinner;
	}
	
	private void fullRevert()
	{
		revert(null);
	}
	
	private void revert(Spinner till)
	{
		// Null check:
		if(view == null)
			return;
		
		while(view.getChildCount() > 0 && view.getChildAt(view.getChildCount() - 1) != till)
		{
			// Remove last spinner
			view.removeViewAt(view.getChildCount() - 1);
			// Remove its label:
			view.removeViewAt(view.getChildCount() - 1);
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
		public View getView (int position, View convertView, ViewGroup parent)
		{
			TextView v = (TextView) super.getView(position, null, parent);
			if((nonSelectableItem != null && position == 0) || (nullItem != null && position == getCount() - 1))
				v.setGravity(Gravity.CENTER); // Centre the text on the nonSelectedDefault item and on the nullItem
			return v;
		}
		
		@Override
		public View getDropDownView (int position, View convertView, ViewGroup parent)
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
