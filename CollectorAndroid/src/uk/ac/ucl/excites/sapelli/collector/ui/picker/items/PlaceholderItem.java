package uk.ac.ucl.excites.sapelli.collector.ui.picker.items;

import android.content.Context;
import android.view.View;

public class PlaceholderItem extends Item
{

	public PlaceholderItem()
	{
		visible = false; //!!!
	}
	
	@Override
	public void setVisibility(boolean visible)
	{
		//do nothing (placeholder can never be visible)
	}

	@Override
	protected View createView(Context context)
	{
		return new View(context);
	}

}
