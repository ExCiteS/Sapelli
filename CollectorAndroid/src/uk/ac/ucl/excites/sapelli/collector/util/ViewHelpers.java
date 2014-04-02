/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.util;

import android.annotation.TargetApi;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.view.View;

/**
 * @author mstevens
 *
 */
public final class ViewHelpers
{

	private ViewHelpers() {}

	static public void setViewBackground(View view, Drawable background)
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN /* = 16 */)
			setViewBackgroundDrawable16AndUp(view, background);
		else
			setViewBackgroundDrawablePre16(view, background);
	}
	
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN)
	static private void setViewBackgroundDrawable16AndUp(View view, Drawable background)
	{
		view.setBackground(background);
	}
	
	@SuppressWarnings("deprecation")
	static private void setViewBackgroundDrawablePre16(View view, Drawable background)
	{
		view.setBackgroundDrawable(background);
	}
	
}
