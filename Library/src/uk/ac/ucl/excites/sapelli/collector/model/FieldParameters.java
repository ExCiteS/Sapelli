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

package uk.ac.ucl.excites.sapelli.collector.model;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.shared.util.Parameters;

/**
 * @author mstevens
 *
 */
public class FieldParameters extends Parameters
{
	
	/**
	 * Empty & immutable FieldParameters instance 
	 */
	static public FieldParameters EMPTY = new FieldParameters(Collections.<String, String> emptyMap());
	
	private final Map<String, String> keyValuePairs;
	
	private FieldParameters(Map<String, String> map)
	{
		this.keyValuePairs = map;
	}
	
	public FieldParameters()
	{
		this(new HashMap<String, String>());
	}
	
	/**
	 * Copy constructor. Creates mutable FieldParameters instance, even if copied from the EMPTY instance.
	 * 
	 * @param other
	 */
	public FieldParameters(FieldParameters other)
	{
		this(new HashMap<String, String>(other.keyValuePairs));
	}
	
	@Override
	public String getValue(String param)
	{
		return keyValuePairs.get(param);
	}
	
	@Override
	public boolean contains(String key)
	{
		return keyValuePairs.containsKey(key);
	}
	
	/**
	 * Sets the value for the specified key and returns the previous value if the key was present before, or null if it was not. 
	 * 
	 * Will throw UnsupportedOperationException when called on the static EMPTY FieldParameters instance.
	 * 
	 * @param key
	 * @param value
	 * @return
	 */
	public String put(String key, String value)
	{	
		return keyValuePairs.put(key, value);
	}
	
	public String remove(String key)
	{
		return keyValuePairs.remove(key);
	}
	
	/**
	 * Warning don't clear arguments from the FieldParameters kept by Field or Trigger instances! Create copy first.
	 * 
	 * @param key
	 * @return the previously assigned value, or null if there was none.
	 */
	public String clear(String key)
	{
		return keyValuePairs.remove(key);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof FieldParameters)
			return this.keyValuePairs.equals(((FieldParameters) obj).keyValuePairs);
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		return keyValuePairs.hashCode();
	}

}
