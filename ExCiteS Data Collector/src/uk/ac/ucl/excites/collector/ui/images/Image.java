/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.images;

import android.widget.ImageView;

/**
 * @author mstevens
 *
 */
public abstract class Image
{
	
	private boolean visible = true;
	
	public abstract void setIn(ImageView imageView);

	public void setVisibility(boolean visible)
	{
		this.visible = visible;
	}
	
	public boolean isVisible()
	{
		return visible;
	}
	
}
