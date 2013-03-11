/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.images;

import android.widget.ImageView;

/**
 * @author mstevens
 *
 */
public class ResourceImage extends Image
{

	private int resourceID;
	
	public ResourceImage(int id)
	{
		this.resourceID = id;
	}
	
	@Override
	protected void _setIn(ImageView imageView)
	{
		imageView.setImageResource(resourceID);
	}

}
