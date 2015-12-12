/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

import org.json.JSONArray;
import org.json.JSONObject;

public final class JSONHelpers
{

	private JSONHelpers() {}
	
	public static boolean contains(JSONArray array, Object obj)
	{
		if(array == null)
			return false;
		if(obj == null)
			obj = JSONObject.NULL;
		for(int i = 0; i < array.length(); i++)
			if(obj.equals(array.opt(i)))
				return true;
		return false;
	}

}
