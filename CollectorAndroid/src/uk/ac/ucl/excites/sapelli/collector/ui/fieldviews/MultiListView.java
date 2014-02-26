package uk.ac.ucl.excites.sapelli.collector.ui.fieldviews;

import uk.ac.ucl.excites.sapelli.collector.project.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.annotation.SuppressLint;
import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.LinearLayout;
import android.widget.Spinner;
import android.widget.TextView;

/**
 * A FieldUI/View class for MultiListFields
 * 
 * @author mstevens
 */
@SuppressLint("ViewConstructor")
public class MultiListView extends LinearLayout implements FieldUI
{

	static private final LayoutParams FULL_WIDTH = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
	static private final String PLEASE_SELECT = "� Please select �"; //TODO multilang
	
	private MultiListField field;
		
	public MultiListView(Context context, MultiListField field)
	{
		super(context);
		this.field = field;
		setOrientation(LinearLayout.VERTICAL);
		
		addNextList(field.getItemsRoot());
	}
	
	private void addNextList(MultiListItem parentItem)
	{
		// Label:
		TextView label = new TextView(getContext());
		label.setText(field.getLabel(getChildCount() / 2));
		label.setLayoutParams(FULL_WIDTH);
		addView(label);
		
		// Combo box:
		final Spinner spinner = new Spinner(getContext());
		spinner.setLayoutParams(FULL_WIDTH);
		//	Adapter:
		final MultiListAdapter adapter = new MultiListAdapter(getContext(), parentItem);
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
		addView(spinner);
	}
	
	private void revert(Spinner till)
	{
		while(getChildAt(getChildCount() - 1) != till)
		{
			// Remove last spinner
			removeViewAt(getChildCount() - 1);
			// Remove its label:
			removeViewAt(getChildCount() - 1);
		}
	}

	@Override
	public MultiListField getField()
	{
		return field;
	}


	@Override
	public void update(Record record)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean isValid(Record record)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void storeValue(Record record)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void cancel()
	{
		// does nothing
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
