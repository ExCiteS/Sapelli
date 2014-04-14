/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * @author mstevens
 *
 */
public class FieldParameters
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
	
	public String get(String key, String defaultValue)
	{	
		if(keyValuePairs.containsKey(key))
			return keyValuePairs.get(key);
		else
			return defaultValue;
	}
	
	public boolean contains(String key)
	{
		return keyValuePairs.containsKey(key);
	}

}
