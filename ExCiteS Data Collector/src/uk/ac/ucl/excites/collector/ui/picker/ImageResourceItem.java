/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker;

import android.content.Context;
import android.view.View;
import android.widget.ImageView;

/**
 * @author mstevens
 *
 */
public class ImageResourceItem extends Item
{

	private int resourceID;
	
	public ImageResourceItem(int id)
	{
		this.resourceID = id;
	}
	
	@Override
	protected View getView(Context context)
	{
		ImageView imageView = new ImageView(context);
		imageView.setImageResource(resourceID);
		return imageView;
	}

}
