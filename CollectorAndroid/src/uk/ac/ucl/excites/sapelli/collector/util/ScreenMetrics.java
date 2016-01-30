/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.util;

import android.content.Context;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.TypedValue;

/**
 * Helper methods to deal with screen resolutions, density and px/dpi conversions 
 * 
 * @author mstevens (class replaced UiUtilities.java by Michalis Vitos)
 */
public final class ScreenMetrics
{

	static public final String TAG = "Metrics";
	
	private ScreenMetrics() {}
	
	/**
	 * Returns the screen density in DPI
	 * 
	 * @param context  the current Context
	 * @return density of the screen in DPI
	 */
	public static int GetScreenDensity(Context context)
	{
		DisplayMetrics metrics = context.getResources().getDisplayMetrics();
		return metrics.densityDpi;
	}
	
	/**
	 * Returns the density scale factor for this screen
	 * 
	 * @param context  the current Context
	 * @return density scale factor of the screen
	 */
	public static float GetScreenDensityScaleFactor(Context context)
	{
		DisplayMetrics metrics = context.getResources().getDisplayMetrics();
		return metrics.density;
	}
	
	/**
	 * Returns the width of the screen in pixels
	 * 
	 * @param context  the current Context
	 * @return width of the screen in pixels
	 */
	public static int GetScreenWidth(Context context)
	{
		DisplayMetrics metrics = context.getResources().getDisplayMetrics();
		return metrics.widthPixels;
	}
	
	/**
	 * Returns the height of the screen in pixels
	 * 
	 * @param context  the current Context
	 * @return height of the screen in pixels
	 */
	public static int GetScreenHeight(Context context)
	{
		DisplayMetrics metrics = context.getResources().getDisplayMetrics();
		return metrics.heightPixels;
	}

	/** Converts the given number of pixels to the corresponding number of density independent pixels using the formula: dip = px / (dpi / 160).
	 * 
	 * @param context  the current Context
	 * @param pixels  a value in Pixels
	 * @return the corresponding value in DIPs
	 * */
	public static float ConvertPxToDip(Context context, float pixels)
	{
		DisplayMetrics dm = context.getResources().getDisplayMetrics();
		return pixels / dm.density; // dm.density gives the scaling factor (= dm.densityDpi / DisplayMetrics.DENSITY_DEFAULT = dm.densityDpi / 160)
	}

	/** Converts the given number of density independent pixels to the corresponding number of pixels using the formula: px = dip * (dpi / 160).
	 * 
	 * @param context  the current Context
	 * @param dips  a value in DIPs
	 * @return the corresponding value in Pixels
	 * */
	public static int ConvertDipToPx(Context context, float dips)
	{
		return (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dips, context.getResources().getDisplayMetrics());
	}
	
	public static void LogDisplayMetrics(Context context)
	{
		Log.i(TAG, "Display metrics: resolution " + GetScreenWidth(context) + " px * " + GetScreenHeight(context) + " px; density: " + GetScreenDensity(context) + " dpi; density scale factor: " + GetScreenDensityScaleFactor(context)); 
	}
	
}
