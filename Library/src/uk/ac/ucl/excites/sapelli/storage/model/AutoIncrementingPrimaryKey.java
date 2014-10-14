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

package uk.ac.ucl.excites.sapelli.storage.model;

import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;

/**
 * As special kind of {@link PrimaryKey} which comprises of a single IntegerColumn whose values are generated automatically (i.e. auto-incrementing) by the back-end database.
 * 
 * @author mstevens
 */
public class AutoIncrementingPrimaryKey extends PrimaryKey
{

	private static final long serialVersionUID = 2L;
	
	/**
	 * @param name
	 * @param intColumn
	 */
	public AutoIncrementingPrimaryKey(String name, IntegerColumn intColumn)
	{
		super(name, intColumn);
	}
	
	public IntegerColumn getColumn()
	{
		return (IntegerColumn) getColumn(0);
	}

	@Override
    public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof AutoIncrementingPrimaryKey)
			return super.equals(obj);
		return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + "AutoIncrement".hashCode(); // to differentiate from a normal PrimaryKey
		return hash;
	}
	
}
