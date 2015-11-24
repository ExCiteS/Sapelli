package uk.ac.ucl.excites.sapelli.shared.util.android;

import java.util.Arrays;
import java.util.List;

import android.content.Context;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.SpinnerAdapter;
import android.widget.TextView;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;

/**
 * Custom ArrayAdapter to allow simulation of the spinner being in an "unselected" state (not supported on Android).
 * 
 * @author mstevens
 * 
 * @see <a href="http://stackoverflow.com/questions/9863378">http://stackoverflow.com/questions/9863378</a>
 */
public class AdvancedSpinnerAdapter<I> extends ArrayAdapter<I> implements SpinnerAdapter
{
	
	// STATIC -------------------------------------------------------
	static protected final int TEXTVIEW_RESOURCE_ID_WHOLE_LAYOUT = 0;
	
	static private final Object DUMMY_TAG = new Object();
	
	static private final float DEFAULT_ITEM_DRAWABLE_PADDING_DP = 3.0f; // dp
	
	// DYNAMIC ------------------------------------------------------
	private String unselectedString;
	private String deselectString;
	
	private float itemDrawablePadding = DEFAULT_ITEM_DRAWABLE_PADDING_DP;
		
	private final LayoutInflater inflater;
	
	/**
	 * Same as mResource field in super class, but it's private and we don't want to use
	 * reflection because variable name could change in future Android versions.
	 */
	private final int itemLayoutResourceId;
	
	/**
	 * Same as mFieldId field in super class, but it's private and we don't want to use
	 * reflection because variable name could change in future Android versions.
	 */
	private final int textViewResourceId;
	
	/**
	 * Same as mDropDownResource field in super class, but it's private and we don't want to use
	 * reflection because variable name could change in future Android versions.
	 */
	private int dropdownItemLayoutResourceId;
	
	public AdvancedSpinnerAdapter(Context context, String unselectedString, String deselectString, I[] items)
	{
		this(context, unselectedString, deselectString, Arrays.asList(items));
	}
	
	/**
	 * @param context
	 * @param itemLayoutResourceId The resource ID for a layout file containing a layout to use when instantiating views.
	 * @param dropdownItemLayoutResourceId resource the layout resource defining the drop down views
	 * @param textViewResourceId The id of the TextView within the layout resource to be populated (or 0 if the whole resource is a TextView)
	 * @param unselectedString
	 * @param deselectString
	 * @param items
	 */
	public AdvancedSpinnerAdapter(Context context, int itemLayoutResourceId, int dropdownItemLayoutResourceId, int textViewResourceId, String unselectedString, String deselectString, I[] items)
	{
		this(context, itemLayoutResourceId, dropdownItemLayoutResourceId, textViewResourceId, unselectedString, deselectString, Arrays.asList(items));
	}
	
	/**
	 * @param context
	 * @param unselectedString
	 * @param deselectString
	 * @param items
	 */
	public AdvancedSpinnerAdapter(Context context, String unselectedString, String deselectString, List<I> items)
	{
		this(context, android.R.layout.simple_spinner_item, android.R.layout.simple_spinner_dropdown_item, TEXTVIEW_RESOURCE_ID_WHOLE_LAYOUT, unselectedString, deselectString, items);
	}
	
	/**
	 * @param context
	 * @param itemLayoutResourceId The resource ID for a layout file containing a layout to use when instantiating views.
	 * @param dropdownItemLayoutResourceId resource the layout resource defining the drop down views
	 * @param textViewResourceId The id of the TextView within the layout resource to be populated (or 0 if the whole resource is a TextView)
	 * @param unselectedString
	 * @param deselectString
	 * @param items
	 */
	public AdvancedSpinnerAdapter(Context context, int itemLayoutResourceId, int dropdownItemLayoutResourceId, int textViewResourceId, String unselectedString, String deselectString, List<I> items)
	{
		super(context, itemLayoutResourceId, textViewResourceId);
		this.itemLayoutResourceId = itemLayoutResourceId;
		this.textViewResourceId = textViewResourceId;
		this.setDropDownViewResource(dropdownItemLayoutResourceId);
		
		this.inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		
		this.unselectedString = unselectedString;
		if(unselectedString != null)
			// Insert the "unselected" item:
			this.add(null);
		
		// Add "real" items:
		for(I item : items)
			this.add(item); // Not using this.addAll(...) because it requires API level 11 (current minimum is 10)
		
		// If preSelect=true, but the field is optional...
		this.deselectString = deselectString;
		if(deselectString != null)
		{	// insert "null-selection" item such that "not answering" is possible:
			this.add(null);
		}
	}

	/* (non-Javadoc)
	 * @see android.widget.ArrayAdapter#setDropDownViewResource(int)
	 */
	@Override
	public void setDropDownViewResource(int resource)
	{
		this.dropdownItemLayoutResourceId = resource;
		super.setDropDownViewResource(resource);
	}

	/**
	 * @return the itemDrawablePadding
	 */
	public float getItemDrawablePadding()
	{
		return itemDrawablePadding;
	}

	/**
	 * @param itemDrawablePadding the itemDrawablePadding to set
	 */
	public void setItemDrawablePadding(float itemDrawablePadding)
	{
		this.itemDrawablePadding = itemDrawablePadding;
	}

	/* (non-Javadoc)
	 * @see android.widget.ArrayAdapter#getItem(int)
	 */
	@Override
	public I getItem(int position)
	{
		if(position < 0)
			return null;
		if(isUnselected(position) || isDeselect(position))
			return null;
		else
			return super.getItem(position);
	}
	
	protected final boolean isUnselected(int position)
	{
		return (unselectedString != null && position == 0);
	}
	
	protected final boolean isDeselect(int position)
	{
		return (deselectString != null && position == getCount() - 1);
	}

	/**
	 * @param position
	 * @param item the item, may be null
	 * @return
	 */
	protected final CharSequence getItemText(int position, I item)
	{
		if(isUnselected(position))
			return unselectedString;
		else if(isDeselect(position))
			return deselectString;
		else if(item instanceof CharSequence)
			return (CharSequence) item;
		else if(item == null)
			return unselectedString;
		else
			return getItemString(item);
	}
	
	/**
	 * @param item (never null)
	 * @return
	 */
	protected String getItemString(I item)
	{
		return item.toString();
	}
	
	/**
	 * Can be overridden to place an icon to the left/start side of the item text
	 * 
	 * @param position
	 * @param item the item, may be null
	 * @return the resource id of the icon drawable, or null if no icon should be used (always return null if item is null!)
	 */
	protected Integer getItemDrawableResourceId(int position, I item)
	{
		return null; // not using icons by default
	}
	
	@Override
	public View getView(int position, View convertView, ViewGroup parent)
	{
		I item = getItem(position);
		return createView(position, item, getItemText(position, item), getItemDrawableResourceId(position, item), isUnselected(position) || isDeselect(position), convertView, parent, itemLayoutResourceId);
	}

	@Override
	public View getDropDownView(int position, View convertView, ViewGroup parent)
	{
		if(isUnselected(position))
		{	// Hide the unselected item:
			TextView dummyView = new TextView(getContext());
			dummyView.setTag(DUMMY_TAG);
			dummyView.setHeight(0);
			//dummyView.setVisibility(View.GONE); //does not seem to make a difference
			return dummyView;
		}
		else
		{
			I item = getItem(position);
			return createView(position, item, getItemText(position, item), getItemDrawableResourceId(position, item), isDeselect(position), convertView, parent, dropdownItemLayoutResourceId);
		}
	}

	/**
	 * @param itemText
	 * @param itemDrawableResourceId
	 * @param center
	 * @param convertView
	 * @param parent
	 * @param resource
	 * @return
	 * 
	 * @see Based on ArrayAdapter#createViewFromResource(int,View,ViewGroup,int)
	 */
	protected View createView(int position, I item, CharSequence itemText, Integer itemDrawableResourceId, boolean center, View convertView, ViewGroup parent, int resource)
	{
		View view;
		TextView textView;
		if(convertView == null || convertView.getTag() == DUMMY_TAG)
			view = inflater.inflate(resource, parent, false);
		else
			view = convertView;
		try
		{
			if(view instanceof TextView || textViewResourceId == TEXTVIEW_RESOURCE_ID_WHOLE_LAYOUT)
				// If no custom field is assigned, assume the whole resource is a TextView
				textView = (TextView) view;
			else
				// Otherwise, find the TextView field within the layout
				textView = (TextView) view.findViewById(textViewResourceId);
		}
		catch(ClassCastException e)
		{
			Log.e(getClass().getSimpleName(), "You must supply a resource ID for a TextView");
			throw new IllegalStateException(getClass().getSimpleName() + " requires the resource ID to be a TextView", e);
		}
		// Set text:
		textView.setText(itemText);
		// Set/remove drawable:
		ViewHelpers.setCompoundDrawablesRelativeWithIntrinsicBounds(textView, itemDrawableResourceId != null ? itemDrawableResourceId : 0, 0, 0, 0);
		// Set drawable padding:
		textView.setCompoundDrawablePadding(ScreenMetrics.ConvertDipToPx(getContext(), itemDrawablePadding));
		// Set/reset gravity:
		textView.setGravity(center ? Gravity.CENTER : Gravity.CENTER_VERTICAL | ViewHelpers.getStartGravity());

		return view;
	}

}
