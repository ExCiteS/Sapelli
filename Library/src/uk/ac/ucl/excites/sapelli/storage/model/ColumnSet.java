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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.model.indexes.Index;
import uk.ac.ucl.excites.sapelli.storage.util.DuplicateColumnException;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * An ordered set of {@link Column}s, possibly with virtual versions.
 * Super class for {@link Schema}, {@link Index}, etc.
 * 
 * @author mstevens
 */
public class ColumnSet implements Serializable
{

	// Statics------------------------------------------------------------
	private static final long serialVersionUID = 2L;
	
	static protected final int UNKNOWN_COLUMN_POSITION = -1;

	// Dynamics-----------------------------------------------------------
	protected final String name;
	protected final boolean useVirtualVersions;
	private boolean sealed = false;
	
	public ColumnSet(String name, boolean useVirtualVersions)
	{
		this.name = name;
		this.useVirtualVersions = useVirtualVersions;
	}
	
	/**
	 * List of all (and only) non-virtual ("real") columns, in order of addition
	 */
	private final List<Column<?>> realColumns = new ArrayList<Column<?>>();
	
	/**
	 * Name to position mapping for all (and only) non-virtual ("real") columns
	 */
	private final Map<String, Integer> columnNameToPosition = new HashMap<String, Integer>();

	/**
	 * Name to column mapping for all (and only) virtual columns
	 */
	private Map<String, VirtualColumn<?, ?>> virtualColumnsByName;
	
	/**
	 * Contains both non-virtual ("real") and virtual columns,
	 * in order of addition with virtual columns following their "real" owner and preceding the next "real" column
	 */
	private transient List<Column<?>> allColumns;
	
	/**
	 * Add a series of new, non-virtual columns to the ColumnSet. The columns' virtual versions are added only if {@link #useVirtualVersions} is {@code true}.
	 * 
	 * @param columns the columns to add, cannot include {@link VirtualColumn}s
	 * @throws DuplicateColumnException in case of a name-clash
	 * @throws IllegalArgumentException
	 */
	public final void addColumns(List<Column<?>> columns) throws DuplicateColumnException, IllegalArgumentException
	{
		addColumns(columns, false); // don't seal yet
	}
	
	/**
	 * Add a series of new, non-virtual columns to the ColumnSet. The columns' virtual versions are added only if {@link #useVirtualVersions} is {@code true}.
	 * 
	 * @param columns the columns to add, cannot include {@link VirtualColumn}s
	 * @param seal if {@code true} the ColumnSet will be sealed after adding the column (i.e. this is the last column to be added)
	 * @throws DuplicateColumnException in case of a name-clash
	 * @throws IllegalArgumentException
	 */
	public final void addColumns(List<Column<?>> columns, boolean seal) throws DuplicateColumnException, IllegalArgumentException
	{
		for(Column<?> c : columns)
			addColumn(c);
		if(seal)
			seal();
	}
	
	/**
	 * Add a new, non-virtual column to the ColumnSet. The column's virtual versions are added only if {@link #useVirtualVersions} is {@code true}.
	 * 
	 * @param column the column to add, cannot be a {@link VirtualColumn}
	 * @return the added column
	 * @throws DuplicateColumnException in case of a name-clash
	 * @throws IllegalArgumentException
	 */
	public final <C extends Column<T>, T> C addColumn(C column) throws DuplicateColumnException, IllegalArgumentException
	{
		return addColumn(column, false); // don't seal yet
	}
	
	/**
	 * Add a new, non-virtual column to the ColumnSet. The column's virtual versions are added only if {@link #useVirtualVersions} is {@code true}.
	 * 
	 * @param column the column to add, cannot be a {@link VirtualColumn}
	 * @param seal if {@code true} the ColumnSet will be sealed after adding the column (i.e. this is the last column to be added)
	 * @return the added column
	 * @throws DuplicateColumnException in case of a name-clash
	 * @throws IllegalArgumentException
	 */
	public final <C extends Column<T>, T> C addColumn(C column, boolean seal) throws DuplicateColumnException, IllegalArgumentException
	{
		return addColumn(column, useVirtualVersions, seal);
	}
	
	/**
	 * Add a new, non-virtual column to the ColumnSet.
	 * 
	 * @param column the column to add, cannot be a {@link VirtualColumn}
	 * @param useVirtualVersion whether or not to also add the column's virtual versions (passing {@code true} will also have an effect if {@link #useVirtualVersions} is also {@code true})
	 * @param seal if {@code true} the ColumnSet will be sealed after adding the column (i.e. this is the last column to be added)
	 * @return the added column
	 * @throws DuplicateColumnException in case of a name-clash
	 * @throws IllegalArgumentException
	 */
	public <C extends Column<T>, T> C addColumn(C column, boolean useVirtualVersions, boolean seal) throws DuplicateColumnException, IllegalArgumentException
	{
		// Add the column:
		addRealColumn(column); // throws if exception if sealed, duplicate, null column, etc.
		
		// If we are allowed then add any virtual versions it may have:		
		if(this.useVirtualVersions && useVirtualVersions)
			for(VirtualColumn<?, T> vCol : column.getVirtualVersions())
			{
				if(!containsColumn(vCol.getSourceColumn()))
					throw new IllegalArgumentException("The schema does not contain the source column (" + vCol.getSourceColumn().getName() + ") of the given virtual column.");
				if(virtualColumnsByName == null)
					virtualColumnsByName = new HashMap<String, VirtualColumn<?, ?>>();
				virtualColumnsByName.put(vCol.getName(), vCol);
			}
		
		// Seal if needed:
		if(seal)
			seal();
		
		// Return added column:
		return column;
	}
	
	protected void addRealColumn(Column<?> column)
	{
		// Checks:
		if(sealed)
			throw new IllegalStateException("Cannot extend a sealed schema!");
		if(column == null)
			throw new NullPointerException("Cannot add null column!");
		if(column instanceof VirtualColumn<?, ?>)
			throw new IllegalArgumentException("Cannot directly add a virtual columns to a schema!");
		if(containsColumn(column.name, true))
			throw new DuplicateColumnException(column.getName());
		
		// Add the column:
		columnNameToPosition.put(column.getName(), realColumns.size());
		realColumns.add(column);
	}
	
	/**
	 * Seals the ColumnSet. After this, ValueSets can be created based on the ColumnSet, but no more columns can be added.
	 * 
	 * @throws IllegalStateException
	 */
	public final void seal() throws IllegalStateException
	{
		if(sealed)
			return; // already sealed
		if(realColumns.isEmpty())
			throw new IllegalStateException("A ColumnSet must contain at least 1 (real) column before it can be sealed.");
		// Do additional work (if any):
		sealTasks();
		// Seal the ColumnSet:
		this.sealed = true;
	}
	
	/**
	 * To be overridden by subclasses that need to perform additional work as part of the sealing of the ColumnSet.
	 */
	protected void sealTasks()
	{
		// does nothing by default
	}
	
	/**
	 * @return whether or not this ColumnSet is sealed
	 */
	public boolean isSealed()
	{
		return sealed;
	}
	
	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}
	
	/**
	 * Gets a column by its name
	 * 
	 * @param name
	 * @param checkVirtual whether or not to look in the schema's virtual columns
	 * @return the {@link Column} instance with this name, or {@code null} if the Schema contains no such column
	 */
	public Column<?> getColumn(String name, boolean checkVirtual)
	{
		Integer pos = columnNameToPosition.get(name);
		if(pos == null)
			return checkVirtual ? getVirtualColumn(name) : null;
		return realColumns.get(pos);
	}
	
	/**
	 * Gets a virtual column by its name
	 * 
	 * @param name
	 * @return the {@link VirtualColumn} instance with this name, or {@code null} if the Schema contains no such column
	 */
	public VirtualColumn<?, ?> getVirtualColumn(String name)
	{
		return virtualColumnsByName != null ? virtualColumnsByName.get(name) : null;
	}
	
	/**
	 * Returns the non-virtual ("real") column at the given position.
	 * Virtual columns cannot be found this way!
	 * 
	 * @param position
	 * @return
	 */
	protected Column<?> getColumn(int position)
	{
		if(position < 0 || position >= realColumns.size())
			throw new ArrayIndexOutOfBoundsException("Invalid column position (" + position + ")");
		return realColumns.get(position);
	}
	
	/**
	 * Returns the position of a non-virtual(!) column with the given name.
	 * Null is returned if the schema does not contain such a column.
	 * Virtual columns cannot be found this way!
	 * 
	 * @param realColumn a non-virtual column
	 * @return	the position of the given {@link Column} instance within this Schema, or {@link #UNKNOWN_COLUMN_POSITION} if the Schema contains no such column.
	 */
	protected int getColumnPosition(String realColumnName)
	{
		Integer pos = columnNameToPosition.get(realColumnName);
		if(pos == null)
			return UNKNOWN_COLUMN_POSITION;
		return pos.intValue();
	}

	/**
	 * Returns a list of all columns (including virtual ones if {@code includeVirtual} is {@code true}) in the order of addition.
	 * If {@code includeVirtual} is {@code true} virtual columns are inserted between their "real" owner and the next "real" column.
	 * 
	 * @param includeVirtual
	 * @return an unmodifiable list of columns
	 */
	public List<Column<?>> getColumns(boolean includeVirtual)
	{
		if(includeVirtual && virtualColumnsByName != null) // includeVirtual=true and we have at least 1 virtual column 
		{
			if(!sealed || allColumns == null) // (re)initialise the allColumns list if it is null or as long as the schema is not sealed.
			{
				allColumns = new ArrayList<Column<?>>(realColumns.size() + virtualColumnsByName.size());
				for(Column<?> nonVirtualCol : realColumns)
				{
					allColumns.add(nonVirtualCol); // "real" column
					// insert virtual versions of real column after it (if they are known by the schema):
					for(VirtualColumn<?, ?> vCol : nonVirtualCol.getVirtualVersions())
						/* check if this vCol was added to the schema (when its "real" owner was added to the schema
						 * through addColumn()), we must check this because the vCol might have been added to its owner
						 * _after_ the latter had been added to the schema).
						 * We use vCol == getColumn(...) instead of containsColumn(vCol) because we that would use equals() instead of ==. */
						if(vCol == getColumn(vCol.getName(), true))
							allColumns.add(vCol);
				}
			}
			return allColumns;
		}
		return Collections.unmodifiableList(realColumns);
	}
	
	/**
	 * @return an unordered collection of the virtual columns in the schema
	 */
	public Collection<VirtualColumn<?, ?>> getVirtualColumns()
	{
		return virtualColumnsByName == null ? Collections.<VirtualColumn<?, ?>> emptyList() : Collections.unmodifiableCollection(virtualColumnsByName.values());
	}

	/**
	 * Checks whether the schema contains a non-virtual column with the given name.
	 * 
	 * @param name the name of a column
	 * @return
	 */
	public boolean containsColumn(String name)
	{
		return containsColumn(name, false);
	}
	
	/**
	 * Checks whether the schema contains a column with the given name.
	 * 
	 * @param name the name of a column
	 * @param checkVirtual whether or not to look in the schema's virtual columns
	 * @return whether the schema contains a column with the given name
	 */
	public boolean containsColumn(String name, boolean checkVirtual)
	{
		return columnNameToPosition.containsKey(name) || (checkVirtual && virtualColumnsByName != null && virtualColumnsByName.containsKey(name));
	}
	
	/**
	 * Checks whether the schema contains the given column (checked by object identity; i.e. == and not equals()).
	 * 
	 * @param column
	 * @return whether or not this Schema contains the given Column
	 */
	public boolean containsColumn(Column<?> column)
	{
		return	column != null &&
				(column instanceof VirtualColumn ?
					column == getVirtualColumn(column.name) :
					column == getColumn(column.name, false));
	}
	
	/**
	 * Checks whether the schema contains the given Column or an exact equivalent of it.
	 * 
	 * @param column
	 * @return whether or not this Schema contains the given Column or an exact equivalent of it
	 */
	public boolean containsEquivalentColumn(Column<?> column)
	{
		return getEquivalentColumn(column) != null;
	}
	
	/**
	 * Returns a Column which is exactly equivalent (or even object-identical!) to the given column
	 * and which is contained by the Schema, or null if there is no such column in the Schema.
	 * 
	 * @param column
	 * @return an equivalent column contained by the Schema or null
	 */
	@SuppressWarnings("unchecked")
	public <T> Column<T> getEquivalentColumn(Column<T> column)
	{
		// Try finding the exact same column (object identity): 
		if(containsColumn(column))
			return column; // return the column itself
		// Make sure the column isn't null:
		if(column == null)
			return null;
		// Try finding an equivalent column with the same name:
		Column<T> myColumn = (Column<T>) getColumn(column.getName(), column instanceof VirtualColumn);
		//	Check if the column is really equivalent:
		return column.equals(myColumn, /* name is already checked */ false, true) ? myColumn : null;
	}
	
	public int getNumberOfColumns(boolean includeVirtual)
	{
		return realColumns.size() + (includeVirtual && virtualColumnsByName != null ? virtualColumnsByName.size() : 0);
	}

	/**
	 * @return whether or not the size taken up by binary stored records of this schema varies at run-time (i.e. depending on data input)
	 */
	public boolean isVariableSize()
	{
		return isVariableSize(false, Collections.<Column<?>> emptySet());
	}
	
	/**
	 * @return whether or not the size taken up by binary stored records of this schema varies at run-time (i.e. depending on data input)
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore in the total
	 * @return
	 */
	public boolean isVariableSize(boolean includeVirtual, Set<? extends Column<?>> skipColumns)
	{
		for(Column<?> c : getColumns(includeVirtual))
			if(!skipColumns.contains(c) && c.isVariableSize())
				return true;
		return false;
	}
	
	/**
	 * Returns the minimum effective number of bits a record of this schema takes up when written to a binary representation.
	 * Includes all non-virtual columns in the count.
	 * 
	 * @return
	 */
	public int getMinimumSize()
	{
		return getMinimumSize(false, Collections.<Column<?>>emptySet());
	}
	
	/**
	 * Returns the minimum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore in the total
	 * @return
	 */
	public int getMinimumSize(boolean includeVirtual, Set<? extends Column<?>> skipColumns)
	{
		int total = 0;
		for(Column<?> c : getColumns(includeVirtual))
			if(!skipColumns.contains(c))
				total += c.getMinimumSize();
		return total;
	}
	
	/**
	 * Returns the maximum effective number of bits a record of this schema takes up when written to a binary representation.
	 * Includes all non-virtual columns in the count.
	 * 
	 * @return
	 */
	public int getMaximumSize()
	{
		return getMaximumSize(false, Collections.<Column<?>>emptySet());
	}

	/**
	 * Returns the maximum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore the total
	 * @return
	 */
	public int getMaximumSize(boolean includeVirtual, Set<? extends Column<?>> skipColumns)
	{
		int total = 0;
		for(Column<?> c : getColumns(includeVirtual))
			if(!skipColumns.contains(c))
				total += c.getMaximumSize();
		return total;
	}
	
	@Override
	public String toString()
	{
		return getClass().getSimpleName() + " " + name;
	}
	
	/**
	 * Check for equality
	 * 
	 * @param obj object to compare this one with
	 * @return whether or not the given Object is a Schema with the same ID & version as this one
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		return equals(obj, true, true);
	}

	/**
	 * Check if the provided object is an identical/equivalent Schema. The usageID & usageSubID are always checked, names and columns are optionally checked, descriptions are ignored. 
	 * 
	 * @param obj object to compare this one with
	 * @param checkNames whether or not to compare the names of the schemas and (if checkColumns is true) those of their columns
	 * @param checkColumns whether or not to compare columns (types, sizes, etc., and names if checkNames is true)
	 * @return whether or not the given Object is an identical/equivalent Schema (under the given checking conditions)
	 */
	public boolean equals(Object obj, boolean checkNames, boolean checkColumns)
	{
		if(this == obj) // compare pointers first
			return true;
		if(obj instanceof ColumnSet)
		{
			ColumnSet that = (ColumnSet) obj;
			// Schema & model name:
			if(checkNames && !this.name.equals(that.name))
				return false;
			// Columns:
			if(checkColumns)
			{
				// Check number of (real) columns:
				if(this.realColumns.size() != that.realColumns.size())
					return false;
				// Compare columns:
				Iterator<Column<?>> myCols = this.realColumns.iterator();
				Iterator<Column<?>> otherCols = that.realColumns.iterator();
				while(myCols.hasNext() /* && otherCols.hasNext() */)
					if(!myCols.next().equals(otherCols.next(), checkNames, true))
						return false;
			}
			return true;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (name == null ? 0 : name.hashCode());
		hash = 31 * hash + realColumns.hashCode();
		hash = 31 * hash + (sealed ? 0 : 1);
		return hash;
	}
	
	public void accept(ColumnVisitor visitor)
	{
		accept(visitor, Collections.<Column<?>>emptySet());
	}
	
	public void accept(ColumnVisitor visitor, Set<? extends Column<?>> skipColumns)
	{
		for(Column<?> c : getColumns(visitor.includeVirtualColumns()))
			if(!skipColumns.contains(c))
				c.accept(visitor);
	}

}
