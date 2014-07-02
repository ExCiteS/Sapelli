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
 * Class representing a database index spanning one or more columns.
 * Implemented as a subclass of {@link Schema}.
 * 
 * @author mstevens
 */
public class Index extends Schema
{

	static private final long serialVersionUID = 2L;
	
	private boolean unique;
	
	public Index(String name, boolean unique, Column<?>... columns)
	{
		super(InternalKind.INDEX, name);
		this.unique = unique;
		
		// Add columns (but check if they are not virtual):
		for(Column<?> iCol : columns)
			if(iCol instanceof VirtualColumn)
				throw new IllegalArgumentException("Indexing of virtual columns is not supported!");
			else
				addColumn(iCol); // Note: the columns are not copied, just shared! (columns don't "know" their Schema(s) anyway)
		seal();
	}
	
	@Override
	public <T> void addColumn(Column<T> column)
	{
		addColumn(column, false); // add column but *not* its virtual versions
	}

	/**
	 * @return the unique
	 */
	public boolean isUnique()
	{
		return unique;
	}
	
	@Override
	public String toString()
	{
		return "Index " + name;
	}
	
	@Override
	public void addIndex(Index index, boolean useAsPrimaryKey)
	{
		throw new UnsupportedOperationException("Cannot add indexes to an index");
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + (unique ? 0 : 1);
		return hash;
	}
	
}
