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
import android.graphics.Color;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;

/**
 * A composite Item subclass which consists of any 2 child items being shown underneath one another
 * 
 * @author benelliott, mstevens
 */
public class SplitItem extends Item
{

	static public final int HORIZONTAL = LinearLayout.HORIZONTAL;
	static public final int VERTICAL = LinearLayout.VERTICAL;

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
				// TODO does this actually work for horizontal splits? code looks weird, verify...
				orientation == HORIZONTAL ? 0 : LinearLayout.LayoutParams.MATCH_PARENT, // horizontal -> take whole height
				orientation == HORIZONTAL ? LinearLayout.LayoutParams.MATCH_PARENT : 0, // vertical -> take whole width
				itemWeight);
			//lp.setMargins(left, top, right, bottom); // TODO set spacing
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