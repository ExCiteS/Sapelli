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
