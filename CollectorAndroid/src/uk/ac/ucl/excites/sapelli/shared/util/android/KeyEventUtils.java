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

package uk.ac.ucl.excites.sapelli.shared.util.android;

import android.annotation.TargetApi;
import android.os.Build;
import android.view.KeyEvent;

/**
 * @author mstevens
 *
 */
public final class KeyEventUtils
{

	static public String keyEventCodeToString(KeyEvent event)
	{
		if(Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB_MR1)
			return "Key code: " + event.getKeyCode(); // return as number
		else
			return keyEventCodeToStringAPI12AndUp(event);
	}
	
	@TargetApi(Build.VERSION_CODES.HONEYCOMB_MR1)
	static private String keyEventCodeToStringAPI12AndUp(KeyEvent event)
	{
		return KeyEvent.keyCodeToString(event.getKeyCode());
	}
	
	private KeyEventUtils()
	{
		// should never be instantiated
	}
	
}
