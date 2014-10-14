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

package uk.ac.ucl.excites.sapelli.collector.model.dictionary;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;

/**
 * @author mstevens
 *
 */
public class Dictionary<I extends DictionaryItem>
{

	static public final int UNKNOWN_INDEX = -1;
	static public final List<String> DOC_HEADERS;
	static
	{
		DOC_HEADERS = new ArrayList<String>();
		DOC_HEADERS.add("INDEX");
		DOC_HEADERS.add("VALUE");
	}
	
	protected final Map<I, Integer> itemToIndex;
	protected final List<I> indexed;
	
	public Dictionary()
	{
		this.itemToIndex = new HashMap<I, Integer>();
		this.indexed = new ArrayList<I>();
	}
	
	public void addItem(I item)
	{
		if(item != null && item.getValue() != null && !item.getValue().isEmpty() && !itemToIndex.containsKey(item))
		{
			indexed.add(item); //adds at the end of the list
			itemToIndex.put(item, Integer.valueOf(indexed.size() - 1));
		}
	}
	
	public int lookupIndex(DictionaryItem item)
	{
		Integer idx = itemToIndex.get(item);
		return (idx != null ? idx : UNKNOWN_INDEX);
	}
	
	public boolean contains(DictionaryItem item)
	{
		return lookupIndex(item) != UNKNOWN_INDEX;
	}
	
	public I lookupItem(int index)
	{
		return indexed.get(index);
	}
	
	public List<I> getItems()
	{
		return indexed;
	}
	
	public boolean isEmpty()
	{
		return indexed.isEmpty();
	}
	
	public int size()
	{
		return indexed.size();
	}
	
	protected List<String> getDocHeaders()
	{
		return DOC_HEADERS;
	}

	public String toCSV(String separator)
	{
		StringBuilder bldr = new StringBuilder();
		// Header:
		bldr.append(StringUtils.join(getDocHeaders(), separator));
		bldr.append('\n');
		// Rows:
		int idx = 0;
		for(DictionaryItem item : indexed)
		{
			List<String> cols = new ArrayList<String>();
			cols.add(Integer.toString(idx++));
			cols.add(item.getValue());
			CollectionUtils.addAllIgnoreNull(cols, item.getDocExtras());
			bldr.append(StringUtils.join(cols, separator));
			bldr.append('\n');
		}
		return bldr.toString();
	}
	
	public List<String> serialise(DictionarySerialiser<I> serialiser)
	{
		List<String> stringList = new ArrayList<String>();
		for(I item : indexed)
			stringList.add(serialiser.serialise(item));
		return stringList;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof Dictionary<?>)
		{
			Dictionary<?> that = (Dictionary<?>) obj;
			return	this.itemToIndex.equals(that.itemToIndex) &&
					this.indexed.equals(that.indexed);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + itemToIndex.hashCode();
		hash = 31 * hash + indexed.hashCode();
		return hash;
	}
	
	public interface DictionarySerialiser<I>
	{
		
		public String serialise(I item);
		
	}
	
}
