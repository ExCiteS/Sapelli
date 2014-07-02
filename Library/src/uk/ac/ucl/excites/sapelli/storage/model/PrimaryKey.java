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

/**
 * A class representing a primary key on a schema, implemented as a subclass of {@link Index}.
 * The index is always a unique one and all included columns must be non-optional (i.e. not "nullable").
 * 
 * @author mstevens
 */
public class PrimaryKey extends Index
{
	
	private static final long serialVersionUID = 2L;
	
	public PrimaryKey(String name, Column<?>... columns)
	{
		super(name, true, columns); // always unique!!!
		// Check if non of the columns are optional:
		for(Column<?> idxCol : getColumns(false))
			if(idxCol.isOptional())
				throw new IllegalArgumentException("An primary key cannot contain optional (i.e. nullable) columns!");
	}
	
	@Override
	public String toString()
	{
		return "PrimaryKey " + name;
	}
	
	@Override
    public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof PrimaryKey)
			return super.equals(obj);
		return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + "PrimaryKey".hashCode(); // to differentiate from a normal index
		return hash;
	}
	
}