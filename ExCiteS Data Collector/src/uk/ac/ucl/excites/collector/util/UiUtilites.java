package uk.ac.ucl.excites.collector.util;

import android.content.Context;
import android.util.DisplayMetrics;

/**
 * @author Michalis Vitos
 * 
 */
public class UiUtilites
{
	public UiUtilites() {}

	/**
	 * Convert dp to px
	 * 
	 * @param context
	 * @param dp
	 * @return
	 */
	public static int dpToPx(Context context, int dp)
	{
		DisplayMetrics displayMetrics = context.getResources().getDisplayMetrics();
		int px = Math.round(dp * (displayMetrics.xdpi / DisplayMetrics.DENSITY_DEFAULT));
		return px;
	}

	/**
	 * Convert px to dp
	 * 
	 * @param context
	 * @param px
	 * @return
	 */
	public int pxToDp(Context context, int px)
	{
		DisplayMetrics displayMetrics = context.getResources().getDisplayMetrics();
		int dp = Math.round(px / (displayMetrics.xdpi / DisplayMetrics.DENSITY_DEFAULT));
		return dp;
	}

}
