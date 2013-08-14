package uk.ac.ucl.excites.collector.ui.picker;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

/**
 * @author Julia, mstevens
 *
 */
public class PickerAdapter extends BaseAdapter
{

	static private final int PADDING = 2; // pixels
	static private final int DEFAULT_BACKGROUND_COLOR = Color.WHITE;
	static private final int DEFAULT_ITEM_HEIGHT = 140;
	static private final int DEFAULT_ITEM_WIDTH = 140;
	static private final ScaleType DEFAULT_SCALE_TYPE = ScaleType.CENTER_INSIDE;
	
	private Context context;
	private int itemWidth;
	private int itemHeight;
	private ScaleType scaleType;
	private int backgroundColor;
	private List<Item> items;
	
	public PickerAdapter(Context localContext)
	{
		this.context = localContext;
		this.itemHeight = DEFAULT_ITEM_HEIGHT;
		this.itemWidth = DEFAULT_ITEM_WIDTH;
		this.scaleType = DEFAULT_SCALE_TYPE;
		this.backgroundColor = DEFAULT_BACKGROUND_COLOR;
		this.items = new ArrayList<Item>();
	}
	
	/**
	 * @param backgroundColor the backgroundColor to set
	 */
	public void setBackgroundColor(int backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}
	
	/**
	 * @param itemHeight the itemHeight to set
	 */
	public void setItemHeight(int itemHeight)
	{
		this.itemHeight = itemHeight;
	}

	/**
	 * @param itemWidth the itemWidth to set
	 */
	public void setItemWidth(int itemWidth)
	{
		this.itemWidth = itemWidth;
	}
	
	/**
	 * @param scaleType the scaleType to set
	 */
	public void setScaleType(ScaleType scaleType)
	{
		this.scaleType = scaleType;
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
			return convertView;
		else
		{
			Item item = items.get(position);
			View view = item.getView(context);
			if(!item.isVisible())
				view.setVisibility(View.INVISIBLE);
			view.setBackgroundColor(backgroundColor);
			view.setLayoutParams(new GridView.LayoutParams(itemWidth, itemHeight));
			view.setPadding(PADDING, PADDING, PADDING, PADDING);
			//Set scaling type for imageviews (type check/cast is not very OO, but acceptable):
			if(view instanceof ImageView)
				((ImageView) view).setScaleType(scaleType);
			return view;
		}
	}

	public void makeInvisible(int position)
	{
		getItem(position).setVisibility(false);
	}
	
	public void makeVisible(int position)
	{
		getItem(position).setVisibility(true);
	}

}
