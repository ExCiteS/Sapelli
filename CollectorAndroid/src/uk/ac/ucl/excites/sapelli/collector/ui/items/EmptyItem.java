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
	
	public EmptyItem()
	{
		this(null);
	}
	
	public EmptyItem(Long id)
	{
		super(id);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.picker.items.Item#createView(android.content.Context)
	 */
	@Override
	protected View createView(Context context)
	{
		return new View(context);
	}

}
