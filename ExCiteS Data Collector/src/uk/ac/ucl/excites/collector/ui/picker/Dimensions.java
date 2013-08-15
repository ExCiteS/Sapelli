/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker;

import uk.ac.ucl.excites.collector.util.ScreenMetrics;
import android.content.Context;


/**
 * Class used to calculate, in a device-independent way, the dimensions of elements of our default PickerView-based UI
 * 
 * 	Example(s):
 * 
 * 	 # Samsung Galaxy Xcover(1):
 * 
 * 		Display specs:
 * 	 	 - Resolution: 320px (w) * 480px (h)
 *  	 - Size: 3.6"
 *  	 - Pixel density: 158ppi
 *  	 - Android-reported display density: 160dpi (scale factor 1) ==> 1dip = 1px
 *  
 *  	UI element sizes:
 *  	 - button height: 60px
 * 		 - item spacing 8px
 * 		 - padding: 2px
 * 		 - on a screen with buttons and 2 columns * 3 rows of items:
 * 		 	- picker item outer area: 156px (w) * 132px (h)
 * 		 	- picker item inner area (padded allround): 152px (w) * 128px (h)
 * 		
 * 		Note:
 * 			For the AP & OIFLEG projects we used images (for 2 * 3 item screens) of 155 px * 135 px
 * 			on the Xcover1. While this which is slightly too big, automatic scaling solves the problem
 * 			without (much) visual quality degradation.
 * 
 * 	 # Samsung Galaxy Xcover2:
 * 
 * 		Display specs:
 * 	 	 - Resolution: 480px (w) * 800px (h)
 *  	 - Size: 4.0"
 *  	 - Pixel density: 233ppi
 *  	 - Android-reported display density: 240dpi (scale factor 1.5) ==> 1dip = 1.5px
 *  
 *  	UI element sizes:
 *  	 - button height: 90px
 * 		 - item spacing 12px
 * 		 - padding: 3px
 * 		 - on a screen with buttons and 2 columns * 3 rows of items:
 * 		 	- picker item outer area: 234px (w) * 224px (h)
 * 		 	- picker item inner area (padded allround): 228px (w) * 218px (h)
 * 
 * Notes:
 *  - TODO: Perhaps later we can extend this class with support for screen rotation
 * 	- Future support for (nested) "frames" (grouping multiple fields in one screen) may affect this class
 * 	- If more dynamic (re)sizing is necessary the class could be given access to the ProjectController to act upon its state
 * 
 * @author mstevens
 */
public final class Dimensions
{
	// Static----------------------------------------------
	
	// the height (in dip) of the buttons on top:
	static public final float BUTTON_HEIGHT_DIP = 60.0f;
	
	// the spacing (in dip) between (horizontally & vertically) picker items:
	static protected final float SPACING_DIP = 8.0f;
	
	// the padding (in dip) _within_ picker items (top, right, bottom, left):
	static private final float PADDING_DIP = 2.0f;
	
	// Dynamic---------------------------------------------
	private Context context;
	
	public Dimensions(Context context)
	{
		this.context = context;
	}
	
	public int getButtonHeightPx()
	{
		return ScreenMetrics.ConvertDipToPx(context, BUTTON_HEIGHT_DIP);
	}
	
	public int getSpacingPx()
	{
		return ScreenMetrics.ConvertDipToPx(context, SPACING_DIP);
	}

	public int getIconWidthPx(int numCols)
	{
		return (ScreenMetrics.GetScreenWidth(context) - ((numCols - 1) * getSpacingPx())) / numCols;
	}
	
	public int getIconHeightPx(int numRows, boolean buttonsShowing)
	{
		return (ScreenMetrics.GetScreenHeight(context) - (buttonsShowing ? (getButtonHeightPx() + getSpacingPx()) : 0) - ((numRows - 1) * getSpacingPx())) / numRows;
	}
	
	public int getPaddingPx()
	{
		return ScreenMetrics.ConvertDipToPx(context, PADDING_DIP);
	}
	
}
