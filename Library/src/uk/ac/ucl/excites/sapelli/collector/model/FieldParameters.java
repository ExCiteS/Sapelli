/**
 * 
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
