/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.GridView;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public class PickerView extends GridView
{

	// Statics:
	static public final int DEFAULT_HEIGHT_PX = 100;
	static public final int DEFAULT_WIDTH_PX = 100;
	
	// Dynamics:
	protected LayoutParams itemLayoutParams = new LayoutParams(DEFAULT_WIDTH_PX, DEFAULT_HEIGHT_PX);
	
	public PickerView(Context context)
	{
		super(context);
		
		setAdapter(new PickerAdapter(context));
		
		// This is needed to hide the border when an picker item is pressed and to calculate the borders more appropriately
		setSelector(R.drawable.picker_view_selector);
	}
	
	public PickerAdapter getAdapter()
	{
		return (PickerAdapter) super.getAdapter();
	}
	
	/**
	 * @param widthPx the widthPx to set
	 * @param heightPx the heightPx to set
	 */
	public void setItemDimensionsPx(int widthPx, int heightPx)
	{
		this.itemLayoutParams = new LayoutParams(widthPx, heightPx);
	}
	
	/**
	 * @author Julia, mstevens
	 *
	 */
	public class PickerAdapter extends BaseAdapter
	{

		private Context context;
		
		private List<Item> items;
		
		public PickerAdapter(Context localContext)
		{
			this.context = localContext;
			this.items = new ArrayList<Item>();
		}
		
		public void addItem(Item item)
		{
			items.add(item);
		}
		
		public void clear()
		{
			items.clear();
		}

		public int getCount()
		{
			return items.size();
		}

		public Item getItem(int position)
		{
			return items.get(position);
		}
		
		public List<Item> getItems()
		{
			return items;
		}

		public long getItemId(int position)
		{
			Item item = getItem(position);
			if(item.hasID())
				return item.getID();
			return position;
		}

		/**
		 * Create a new ImageView for each item referenced by the Adapter
		 */
		public View getView(int position, View convertView, ViewGroup parent)
		{
			if(convertView != null)
			{
				items.get(position).applyVisibility(convertView); // in case visibility has changed
				return convertView;
			}
			else
			{
				// Create the view:
				View view = items.get(position).getView(context);
				
				// Set layout params (width & height):
				view.setLayoutParams(itemLayoutParams);
				
				// Return view:
				return view;
			}
		}
	}
	
}
