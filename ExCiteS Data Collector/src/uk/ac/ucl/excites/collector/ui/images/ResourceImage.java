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
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.ui.images.Image#setIn(android.widget.ImageView)
	 */
	@Override
	public void setIn(ImageView imageView)
	{
		imageView.setImageResource(resourceID);
	}

}
