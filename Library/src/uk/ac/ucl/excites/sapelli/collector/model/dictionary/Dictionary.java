/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.dictionary;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.util.StringUtils;

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
	
	protected Map<I, Integer> itemToIndex;
	protected List<I> indexed;
	
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
	
}
