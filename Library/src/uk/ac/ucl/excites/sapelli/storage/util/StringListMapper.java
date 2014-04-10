/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.util;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn.ValueMapper;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;

/**
 * Helper class which implements {@link ValueMapper} for the common combination
 * of an {@link IntegerColumn} as the sourceColumn and a {@link StringColumn} as
 * the targetColumn of a {@link VirtualColumn}.
 * 
 * @author mstevens
 */
public final class StringListMapper extends ValueMapper<String, Long>
{
	
	private static final long serialVersionUID = 2L;

	private List<String> stringList;
	
	public StringListMapper(List<String> stringList)
	{
		if(stringList == null)
			throw new NullPointerException("stringList cannot be null!");
		this.stringList = stringList;
	}

	@Override
	public String mapValue(Long nonNullValue)
	{
		return stringList.get(nonNullValue.intValue());
	}
	
	public int getMaxStringLength()
	{
		int maxStringLength = 0;
		for(String str : stringList)
			if(str != null && str.length() > maxStringLength)
				maxStringLength = str.length();
		return maxStringLength;
	}
	
	@Override
	public int hashCode()
	{
		return stringList.hashCode();
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof StringListMapper)
			return this.stringList.equals(((StringListMapper) obj).stringList);
		return false;
	}

}
