/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker;

import android.content.Context;
import android.view.View;

/**
 * @author mstevens
 *
 */
public abstract class Item
{
	
	protected boolean visible = true;
	
	protected abstract View getView(Context context);

	public void setVisibility(boolean visible)
	{
		this.visible = visible;
	}
	
	public boolean isVisible()
	{
		return visible;
	}
	
}
