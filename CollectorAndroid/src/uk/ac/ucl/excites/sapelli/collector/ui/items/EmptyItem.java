/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.items;

import android.content.Context;
import android.view.View;

/**
 * @author mstevens
 *
 */
public class EmptyItem extends Item
{

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.picker.items.Item#createView(android.content.Context)
	 */
	@Override
	protected View createView(Context context)
	{
		return new View(context);
	}

}
