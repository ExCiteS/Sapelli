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

package uk.ac.ucl.excites.sapelli.collector.ui.items;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;

/**
 * A composite Item subclass which consists of any 2 child items being shown underneath or next to one another (depending on the 
 * orientation specified). We use the same convention as Android's LinearLayout when describing orientation. That is,
 * <ul>
 * <li>Vertical - child items are stacked on top of one another (adding an item reduces existing items' height) </li>
 * <li>Horizontal - child items are stacked next to one another (adding an item reduces existing items' width) </li>
 * </ul>
 * @author benelliott, mstevens
 */
public class SplitItem extends Item
{
	static public final int HORIZONTAL = LinearLayout.HORIZONTAL;
	static public final int VERTICAL = LinearLayout.VERTICAL;
	static public final int SPLIT_ITEM_SPACING_PX = 2;

	private int orientation;
	private List<WeightedItem> items;

	/**
	 * @param orientation
	 */
	public SplitItem(int orientation)
	{
		this(null, orientation);
	}

	/**
	 * @param id
	 * @param orientation
	 */
	public SplitItem(Integer id, int orientation)
	{
		super(id);
		if(orientation != HORIZONTAL && orientation != VERTICAL)
			throw new IllegalArgumentException("Invalid orientation");
		this.orientation = orientation;
		items = new ArrayList<WeightedItem>();
	}

	/**
	 * @param item
	 * @param weight
	 * @return
	 */
	public SplitItem addItem(Item item, float weight)
	{
		return addItem(item, weight, -1);
	}
	
	/**
	 * @param item
	 * @param weight
	 * @param paddingPx padding applied to item, unless it is = -1
	 * @return
	 */
	public SplitItem addItem(Item item, float weight, int paddingPx)
	{
		items.add(new WeightedItem(item, weight));
		if(paddingPx != -1)
			item.setPaddingPx(paddingPx);
		return this;
	}

	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		LinearLayout container = new LinearLayout(context); // instantiate a LinearLayout to hold the items
		container.setOrientation(orientation);

		// Add items & sum weights:
		float weightSum = 0;
		for(int i = 0, s = items.size(); i < s; i++)
		{
			Item item = items.get(i).item;
			float itemWeight = items.get(i).weight;
			View itemView = item.getView(context, recycleChildren);
			
			// LayoutParams:
			LayoutParams lp = new LayoutParams(
				// set the "malleable" dimension to 0 so that Android decides what to do with it:
				orientation == HORIZONTAL ? 0 : LinearLayout.LayoutParams.MATCH_PARENT, // horizontal -> take whole height
				orientation == HORIZONTAL ? LinearLayout.LayoutParams.MATCH_PARENT : 0, // vertical -> take whole width
				itemWeight);

			// create margins array (top and bottom margins on items if vertical; left and right margins if horizontal)
			int[] margins = orientation == HORIZONTAL ? new int[]{SPLIT_ITEM_SPACING_PX, 0, SPLIT_ITEM_SPACING_PX, 0} : new int[]{0, SPLIT_ITEM_SPACING_PX, 0, SPLIT_ITEM_SPACING_PX};
			// apply margins:
			if(i == 0) //if first item, don't add top or left margin (one of these is 0 anyway depending on orientation)
				lp.setMargins(0, 0, margins[2], margins[3]);
			else if(i == items.size() - 1) // if last item, don't add bottom or right margin (one of these is 0 anyway depending on orientation)
				lp.setMargins(margins[0], margins[1], 0, 0);
			else // if neither, just add whatever margins were specified
				lp.setMargins(margins[0], margins[1], margins[2], margins[3]);
			
			itemView.setLayoutParams(lp);
			
			container.addView(itemView);
			weightSum += itemWeight;
		}

		// Set weight sum (this approach doesn't assume they all sum to 1):
		container.setWeightSum(weightSum);

		return container;
	}

	/**
	 * Convenience class for holding an item and an associated weight.
	 * 
	 * @author benelliott
	 */
	private class WeightedItem
	{
		
		private Item item;
		private float weight;

		private WeightedItem(Item item, float weight)
		{
			this.item = item;
			this.weight = weight;
		}
	}
	
}