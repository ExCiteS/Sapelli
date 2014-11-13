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
	protected boolean recycleViews;
	
	public PickerView(Context context)
	{
		this(context, true); // allow recycled views by default
	}
	
	public PickerView(Context context, boolean recycleViews)
	{
		super(context);
		this.recycleViews = recycleViews;
		
		// Set adapter:
		setAdapter(new PickerAdapter());
		
		// This is needed to hide the border when an picker item is pressed and to calculate the borders more appropriately
		setSelector(R.drawable.picker_view_selector);
	}
	
	public PickerAdapter getAdapter()
	{
		return (PickerAdapter) super.getAdapter();
	}
	
	/**
	 * @return the recycleViews
	 */
	public boolean isRecycleViews()
	{
		return recycleViews;
	}

	/**
	 * @param recycleViews the recycleViews to set
	 */
	public void setRecycleViews(boolean recycleViews)
	{
		this.recycleViews = recycleViews;
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
	 * @author mstevens, Julia
	 *
	 */
	public class PickerAdapter extends BaseAdapter
	{

		private final List<Item> items;
		
		public PickerAdapter()
		{
			this.items = new ArrayList<Item>();
		}
		
		public void addItem(Item item)
		{
			items.add(item);
		}
		
		public void addItemAt(int location, Item item) {
	        items.add(location, item);
        }
		
		public void removeItem(Item item)
		{
			items.remove(item);
		}
		
		public void clear()
		{
			items.clear();
		}

		@Override
		public int getCount()
		{
			return items.size();
		}

		@Override
		public Item getItem(int position)
		{
			return items.get(position);
		}

		@Override
		public long getItemId(int position)
		{
			Item item = getItem(position);
			if(item.hasID())
				return (long) item.getID();
			return position;
		}

		/**
		 * Create a new ImageView for each item referenced by the Adapter
		 */
		@Override
		public View getView(int position, View convertView, ViewGroup parent)
		{
			if(recycleViews && convertView != null && convertView.getId() == getItemId(position))
			{
				items.get(position).applyVisibility(convertView); // in case visibility has changed
				return convertView;
			}
			else
			{
				// Create the view:
				View view = items.get(position).getView(getContext(), recycleViews);
				
				// Set id:
				view.setId((int) getItemId(position));
				
				// Set layout params (width & height):
				view.setLayoutParams(itemLayoutParams);

				// Return view:
				return view;
			}
		}
	}
	
}
