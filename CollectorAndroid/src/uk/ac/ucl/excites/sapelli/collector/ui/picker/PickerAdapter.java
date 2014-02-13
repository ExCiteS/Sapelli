package uk.ac.ucl.excites.sapelli.collector.ui.picker;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.Item;
import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;

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
			return items.get(position).getView(context);
	}

}
