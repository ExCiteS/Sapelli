/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.items;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.widget.ImageView;

/**
 * An Item subclass which draws a Drawable
 * 
 * @author mstevens
 */
public class DrawableItem extends Item
{
	
	private Drawable drawable;
	
	public DrawableItem(Drawable drawable)
	{
		this.drawable = drawable;
	}
	
	@Override
	protected View createView(Context context)
	{
		ImageView view = new ImageView(context);
		view.setImageDrawable(drawable);
		return view;
	}

}
