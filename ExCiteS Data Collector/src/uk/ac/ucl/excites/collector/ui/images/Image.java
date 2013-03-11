/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.images;

import android.view.View;
import android.widget.ImageView;

/**
 * @author mstevens
 *
 */
public abstract class Image
{
	
	protected boolean visible = true;
	
	public void setIn(ImageView imageView)
	{
		if(visible)
			_setIn(imageView);
		else
			imageView.setVisibility(View.INVISIBLE);
	}
	
	protected abstract void _setIn(ImageView imageView);

	public void setVisibility(boolean visible)
	{
		this.visible = visible;
	}
	
	public boolean isVisible()
	{
		return visible;
	}
	
}
