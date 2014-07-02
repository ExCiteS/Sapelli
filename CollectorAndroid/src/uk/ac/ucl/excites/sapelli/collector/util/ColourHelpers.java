/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
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
