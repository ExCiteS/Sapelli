/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.storage.model.indexes;

import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * A class representing a primary key on a schema, implemented as a subclass of {@link Index}.
 * The index is always a unique one and all included columns must be non-optional (i.e. not "nullable").
 * 
 * @author mstevens
 */
public class PrimaryKey extends Index
{
	
	// STATICS-------------------------------------------------------
	static private final long serialVersionUID = 2L;

	static public final String COLUMN_NAME_SEPARATOR = "_";
	
	static public PrimaryKey WithColumnNames(Column<?>... columns)
	{
		if(columns == null || columns.length == 0)
			throw new IllegalArgumentException("Primary key needs to span at least 1 column");
		TransactionalStringBuilder bldr = new TransactionalStringBuilder(COLUMN_NAME_SEPARATOR);
		for(Column<?> c : columns)
			bldr.append(c.name);
		return new PrimaryKey(bldr.toString(), columns);
	}
	
	// DYNAMICS------------------------------------------------------
	public PrimaryKey(String name, Column<?>... columns)
	{
		super(name, true, columns); // always unique!!!
		// Check if we have at least 1 column:
		if(columns == null || columns.length == 0)
			throw new IllegalArgumentException("Primary key needs to span at least 1 column");
		// Check if none of the columns are optional or lossy:
		for(Column<?> idxCol : getColumns(false))
		{
			if(!idxCol.isRequired(true))
				throw new IllegalArgumentException("A primary key cannot contain optional (i.e. nullable) (sub)columns!");
			if(idxCol.canBeLossy())
				throw new IllegalArgumentException("A primary key cannot contain lossy (sub)columns!");
		}
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