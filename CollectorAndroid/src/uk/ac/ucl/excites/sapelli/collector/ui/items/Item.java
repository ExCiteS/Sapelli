/**
 * 
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
	protected int paddingPx = DEFAULT_PADDING_PX;
	protected int backgroundColor = DEFAULT_BACKGROUND_COLOR;
	protected boolean visible = true;
	
	private View view = null;
	
	public View getView(Context context)
	{
		if(view == null)
			view = createView(context);
		
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
	
	protected abstract View createView(Context context);
	
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
	
}
