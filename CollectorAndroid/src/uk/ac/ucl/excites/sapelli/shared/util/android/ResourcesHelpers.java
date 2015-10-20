package uk.ac.ucl.excites.sapelli.shared.util.android;

import android.annotation.TargetApi;
import android.content.res.Resources;
import android.content.res.Resources.Theme;
import android.graphics.drawable.Drawable;
import android.os.Build;

/**
 * @author mstevens
 *
 */
public final class ResourcesHelpers
{
	
	private ResourcesHelpers() {}
	
	static public Drawable getDrawable(Resources res, int id)
	{
		return getDrawable(res, id, null);
	}
	
	static public Drawable getDrawable(Resources res, int id, Theme theme)
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP /* = 21 */)
			return getDrawableLollipop(res, id, theme);
		else
			return getDrawablePreLollipop(res, id); // no theme
	}

	@TargetApi(Build.VERSION_CODES.LOLLIPOP)
	static private Drawable getDrawableLollipop(Resources res, int id, Theme theme)
	{
		return res.getDrawable(id, theme);
	}

	@SuppressWarnings("deprecation")
	static private Drawable getDrawablePreLollipop(Resources res, int id)
	{
		return res.getDrawable(id);
	}
	
}
