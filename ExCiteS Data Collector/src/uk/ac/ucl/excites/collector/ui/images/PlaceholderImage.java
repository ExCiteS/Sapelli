package uk.ac.ucl.excites.collector.ui.images;

import android.widget.ImageView;

public class PlaceholderImage extends Image
{

	public PlaceholderImage()
	{
		visible = false; //!!!
	}
	
	@Override
	public void setVisibility(boolean visible)
	{
		//do nothing (placeholder can never be visible)
	}
	
	@Override
	protected void _setIn(ImageView imageView)
	{
		//does nothing
	}

}
