/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.util;

import android.graphics.Color;
import android.util.Log;

/**
 * @author mstevens
 * 
 */
public final class ColourHelpers
{

	static public final String TAG = "ColourHelpers";
	static public final int DEFAULT_COLOR = Color.BLACK;
	
	private ColourHelpers() {}

	/**
	 * @param intendedColour  the intended colour (as String)
	 * @param alternativeColour  an alternative colour (as int) to use if parsing of the intended one failed  
	 * @return
	 */
	static public int ParseColour(String intendedColour, int alternativeColour)
	{
		int colour = alternativeColour;
		try
		{
			colour = Color.parseColor(intendedColour);
		}
		catch(NullPointerException npe)
		{
			Log.w(TAG, "Unable to parse null colour.");
		}
		catch(IllegalArgumentException iae)
		{
			Log.w(TAG, "Unable to parse colour: " + intendedColour);
		}
		return colour;
	}
	
	/**
	 * @param intendedColour  the intended colour (as String)
	 * @param alternativeColour  an alternative colour (as String) to use if parsing of the intended one failed, if parsing the alternative also fails the overall default ({@value #DEFAULT_COLOUR}) is used  
	 * @return
	 */
	static public int ParseColour(String intendedColour, String alternativeColour)
	{
		int altColour = ParseColour(alternativeColour, DEFAULT_COLOR);
		return ParseColour(intendedColour, altColour);
	}

}
