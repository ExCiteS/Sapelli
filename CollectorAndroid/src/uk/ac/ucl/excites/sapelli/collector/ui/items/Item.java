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

import android.content.Context;
import android.graphics.Color;
import android.view.View;

/**
 * An abstract class representing picker items
 * 
 * @author mstevens
 *
 */
public abstract class Item
{
	
	// Static (defaults):
	static public final int DEFAULT_PADDING_PX = 4;
	static public final int DEFAULT_BACKGROUND_COLOR = Color.WHITE;
	
	// Dynamics:
	protected Integer id;
	protected int paddingPx = DEFAULT_PADDING_PX;
	protected int backgroundColor = DEFAULT_BACKGROUND_COLOR;
	protected boolean visible = true;
	protected String description;
	
	private View view = null; // cached view instance, to allow for recycling
	
	/**
	 * @param id (may be null)
	 */
	public Item(Integer id)
	{
		this.id = id;
	}
	
	public View getView(Context context)
	{
		return getView(context, true); // recycle views by default
	}
	
	/**
	 * @param context
	 * @param recycle whether or not to recycle previously instantiate view instance
	 * @return
	 */
	public View getView(Context context, boolean recycle)
	{
		if(view == null || !recycle)
			view = createView(context, recycle);
		
		// Set padding:
		view.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);
		
		// Set background color:
		view.setBackgroundColor(backgroundColor);
		
		// Set view visibility:
		applyVisibility(view);
		
		return view;
	}
	
	public void invalidateView()
	{
		view = null;
	}
	
	/**
	 * @param context
	 * @param recycleChildren only relevant to composite subclasses like {@link LayeredItem}
	 * @return
	 */
	protected abstract View createView(Context context, boolean recycleChildren);
	
	public void applyVisibility(View view)
	{
		view.setVisibility(visible ? View.VISIBLE : View.INVISIBLE);
	}

	public void setVisibility(boolean visible)
	{
		this.visible = visible;
	}
	
	public boolean isVisible()
	{
		return visible;
	}
	
	/**
	 * @return the paddingPx
	 */
	public int getPaddingPx()
	{
		return paddingPx;
	}

	/**
	 * @param paddingPx the paddingPx to set
	 */
	public void setPaddingPx(int paddingPx)
	{
		this.paddingPx = paddingPx;
	}
	
	public boolean hasID()
	{
		return id != null;
	}
	
	public Integer getID()
	{
		return id;
	}

	/**
	 * @return the backgroundColor
	 */
	public int getBackgroundColor()
	{
		return backgroundColor;
	}

	/**
	 * @param backgroundColor the backgroundColor to set
	 */
	public void setBackgroundColor(int backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}

	/**
	 * @return the description
	 */
	public String getDescription()
	{
		return description;
	}

	/**
	 * @param description the description to set
	 */
	public void setDescription(String description)
	{
		this.description = description;
	}
}
