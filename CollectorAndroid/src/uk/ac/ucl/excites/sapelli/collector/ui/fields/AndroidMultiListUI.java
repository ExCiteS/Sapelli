package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;
import android.widget.Spinner;
import android.widget.TextView;

/**
 * Android version of MultiListUI
 * 
 * @author mstevens
 */
public class AndroidMultiListUI extends MultiListUI<View> //extends LinearLayout implements FieldUI
{

	static private final LayoutParams FULL_WIDTH = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
	
	private LinearLayout view;
	
	public AndroidMultiListUI(MultiListField listField, CollectorController controller, CollectorView collectorView)
	{
		super(listField, controller, collectorView);
	}
	
	@Override
	public LinearLayout getPlatformView(boolean onPage, CollectorRecord record)
	{
		if(view == null)
		{
			view = new LinearLayout(((CollectorView) collectorUI).getContext());
			view.setOrientation(LinearLayout.VERTICAL);
		}
		
		// Update...
		addNextList(field.getItemsRoot());
		// TODO set previous value...
		
		return view;
	}
	

	@Override
	protected MultiListItem getChosenItem()
	{
		// TODO Auto-generated method stub
		return null;
	}
	
	private void addNextList(MultiListItem parentItem)
	{
		// Null check:
		if(view == null)
			return;
		
		// Label:
		TextView label = new TextView(view.getContext());
		label.setText(field.getLabel(view.getChildCount() / 2));
		label.setLayoutParams(FULL_WIDTH);
		view.addView(label);
		
		// Combo box:
		final Spinner spinner = new Spinner(view.getContext());
		spinner.setLayoutParams(FULL_WIDTH);
		//	Adapter:
		final MultiListAdapter adapter = new MultiListAdapter(view.getContext(), parentItem);
		spinner.setAdapter(adapter);
		//	Select default if preSelect=true:
		if(field.isPreSelect())
			spinner.setSelection(parentItem.getDefaultChildIndex());
		//else: first (dummy) item will be selected
		//	Item selected event:
		spinner.setOnItemSelectedListener(new OnItemSelectedListener()
		{

			@Override
			public void onItemSelected(AdapterView<?> parent, View view, int position, long id)
			{
				revert(spinner);
				MultiListItem chosen = adapter.getItem(position);
				if(chosen != adapter.nonSelectableItem && !chosen.isLeaf())
					addNextList(chosen);
			}

			@Override
			public void onNothingSelected(AdapterView<?> parent)
			{
				revert(spinner);
			}
			
		});
		view.addView(spinner);
	}
	
	private void revert(Spinner till)
	{
		// Null check:
		if(view == null)
			return;
		
		while(view.getChildAt(view.getChildCount() - 1) != till)
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
				this.add(item);
			// Not using this.addAll(...) because it requires API level 11 (current minimum is 9)
		}
		
		@Override
		public View getView (int position, View convertView, ViewGroup parent)
		{
			TextView v = (TextView) super.getView(position, null, parent);
			if(nonSelectableItem != null && position == 0)
				v.setGravity(Gravity.CENTER); // Centre the text on the nonSelectedDefault item
			return v;
		}
		
		@Override
		public View getDropDownView (int position, View convertView, ViewGroup parent)
		{	//TODO do show the nonSelectableItem when the field is optional? 
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
