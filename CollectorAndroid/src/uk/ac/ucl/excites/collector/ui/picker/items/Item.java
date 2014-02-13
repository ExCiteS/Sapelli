/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker.items;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.widget.GridView;

/**
 * An abstract class representing picker items
 * 
 * @author mstevens
 *
 */
public abstract class Item
{
	
	// Static (defaults):
	static public final int DEFAULT_HEIGHT_PX = 100;
	static public final int DEFAULT_WIDTH_PX = 100;
	static public final int DEFAULT_PADDING_PX = 4;
	static public final int DEFAULT_BACKGROUND_COLOR = Color.WHITE;
	
	// Dynamics:
	protected int widthPx = DEFAULT_WIDTH_PX;
	protected int heightPx = DEFAULT_HEIGHT_PX;
	protected int paddingPx = DEFAULT_PADDING_PX;
	protected int backgroundColor = DEFAULT_BACKGROUND_COLOR;
	protected boolean visible = true;
	
	private View view = null;
	
	public View getView(Context context)
	{
		if(view == null)
			view = createView(context);
		
		// Set width, height & padding:
		view.setLayoutParams(new GridView.LayoutParams(widthPx, heightPx));
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
	 * @return the widthPx
	 */
	public int getWidthPx()
	{
		return widthPx;
	}

	/**
	 * @param widthPx the widthPx to set
	 */
	public void setWidthPx(int widthPx)
	{
		this.widthPx = widthPx;
	}

	/**
	 * @return the heightPx
	 */
	public int getHeightPx()
	{
		return heightPx;
	}

	/**
	 * @param heightPx the heightPx to set
	 */
	public void setHeightPx(int heightPx)
	{
		this.heightPx = heightPx;
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
