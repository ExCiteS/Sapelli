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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;

/**
 * Helper class to locate & point to (sub)Columns in Schemas and RecordColumns
 * 
 * @author mstevens
 *
 * @param <C> the Column type
 */
public class ColumnPointer<C extends Column<?>>
{
	
	// STATIC ---------------------------------------------------------------------------
	/**
	 * If more than one column is passed all but the last one must be ValueSetColumns. The first
	 * column that is passed is assumed to be a top-level column but this cannot be checked in
	 * absence of a schema. Each subsequent column must be a part of the ColumnSet of the previous
	 * ValueSetColumn.
	 * 
	 * @param columns
	 */
	static public ColumnPointer<?> FromArray(Column<?>... columns)
	{
		return FromList(Arrays.asList(columns));
	}
	
	/**
	 * If more than one column is passed all but the last one must be ValueSetColumns. The first
	 * column that is passed is assumed to be a top-level column but this cannot be checked in
	 * absence of a schema. Each subsequent column must be a part of the ColumnSet of the previous
	 * ValueSetColumn.
	 * 
	 * Note: in absence of a topLevelSchema we cannot check if the first column is a top level one
	 * 
	 * @param columns
	 */
	static public ColumnPointer<?> FromList(List<Column<?>> columns)
	{
		if(columns == null || columns.isEmpty())
			throw new IllegalArgumentException("At least column must be given!");
		// Make parents list:
		List<ValueSetColumn<?>> parents = new ArrayList<>(columns.size() - 1);
		int c = 0;
		for(; c < columns.size() - 1; c++)
			if(columns.get(c) instanceof ValueSetColumn)
				parents.add((ValueSetColumn<?>) columns.get(c));
			else
				throw new IllegalArgumentException("Parent columns must be " + ValueSetColumn.class.getSimpleName() + "s!");
		// Return new cp:
		return new ColumnPointer<Column<?>>(parents, columns.get(c));
	}
	
	/**
	 * @param topLevelCS the top-level ColumnSet or Schema
	 * @param columnName the name of the pointed-at column
	 */
	static public ColumnPointer<?> FromName(ColumnSet topLevelCS, String columnName)
	{	
		// Null & empty check:
		if(columnName == null || columnName.isEmpty())
			throw new IllegalArgumentException("columnName cannot be null or empty");
		
		// Build up stack:
		if(columnName.indexOf(ValueSetColumn.QUALIFIED_NAME_SEPARATOR) == -1)
			return new ColumnPointer<Column<?>>(ConstructPathTo(topLevelCS, columnName, true)); // Find by columnName (recursive, depth-first search; columnName will be sanitised if not found as is)
		else
		{
			Stack<Column<?>> columnStack = new Stack<Column<?>>();
			// Find by qualified name:
			ColumnSet columnSet = topLevelCS;
			for(String colName : columnName.split("\\" + ValueSetColumn.QUALIFIED_NAME_SEPARATOR))
			{
				// Deal with previous (record)column:
				if(!columnStack.isEmpty())
					columnSet = ((ValueSetColumn<?>) columnStack.peek()).getColumnSet();
				// Deal with current column:
				Column<?> col = columnSet.getColumn(colName, true);
				// If not found...
				if(col == null)
					col = columnSet.getColumn(Column.SanitiseName(colName), true); // ... try again with sanitised name
				// If still not found...
				if(col == null)
					throw new IllegalArgumentException("Column \"" + columnName + "\" not found in " + topLevelCS.toString());
				// Found:
				columnStack.push(col);
			}
			return new ColumnPointer<Column<?>>(columnStack);
		}
	}
	
	/**
	 * Constructs a new path to the given column, or an equivalent one if allowEquivalent is true.
	 * 
	 * @param topLevelCS
	 * @param column
	 * @param allowEquivalent whether to allow pointing to a column equivalent to the given one (reachable from the topLevelSchema), or only the exact given column
	 * @return a path to the column
	 * @throws IllegalArgumentException when no path could be constructed
	 */
	static private Stack<Column<?>> ConstructPathTo(ColumnSet topLevelCS, final Column<?> column, boolean allowEquivalent) throws IllegalArgumentException
	{
		// Find column & construct path to it:
		Stack<Column<?>> path = ConstructPath(
			new Stack<Column<?>>(),
			topLevelCS,
			allowEquivalent ?
				new ColumnMatcher()
				{
					@Override
					public Column<?> findColumnIn(ColumnSet columnSet)
					{
						return columnSet.getEquivalentColumn(column);
					}
				} :
				new ColumnMatcher()
				{
					@Override
					public Column<?> findColumnIn(ColumnSet columnSet)
					{
						if(columnSet.containsColumn(column))
							return column;
						else
							return null;
					}
				});
		// Check if we have a valid path:
		if(path == null || /*just in case*/ path.isEmpty())
			throw new IllegalArgumentException("Column \"" + column.getName() + "\" is not part of " + topLevelCS.toString());
		return path;
	}
	
	/**
	 * Constructs a new path to a column identified only by a name.
	 * 
	 * @param topLevelCS
	 * @param columnName name of the column to construct a path to
	 * @param trySanitising
	 * @return a path to the column
	 * @throws IllegalArgumentException when no path could be constructed
	 */
	static private Stack<Column<?>> ConstructPathTo(ColumnSet topLevelCS, final String columnName, boolean trySanitising) throws IllegalArgumentException
	{
		// Find column & construct path to it:
		Stack<Column<?>> path = ConstructPath(new Stack<Column<?>>(), topLevelCS, new ColumnMatcher()
		{
			@Override
			public Column<?> findColumnIn(ColumnSet columnSet)
			{
				return columnSet.getColumn(columnName, true); // we allow virtual columns
			}
		});
		// Check if we have a valid path:
		if(path == null || /*just in case*/ path.isEmpty())
		{
			if(trySanitising) // try again with sanitised name if allowed:
				try
				{
					return ConstructPathTo(topLevelCS, Column.SanitiseName(columnName), false); // pass false to avoid endless sanitation loop ;-)
				}
				catch(IllegalArgumentException iae) { /* exception is thrown below */ }
			throw new IllegalArgumentException("Column \"" + columnName + "\"" + (trySanitising ? " (or \"" + Column.SanitiseName(columnName) + "\")" : "") + " not found in " + topLevelCS.toString());
		}
		return path;
	}
	
	/**
	 * Recursive method to construct a path to a column identified using the given ColumnMatcher
	 * Uses depth-first traversal.
	 *   
	 * @param path Stack to build the path on
	 * @param columnSet
	 * @param matcher
	 * @return a complete path to the column matched by the matcher, or null if it could not be found
	 */
	static private Stack<Column<?>> ConstructPath(Stack<Column<?>> path, ColumnSet columnSet, ColumnMatcher matcher)
	{
		// Try to find the column in the current schema (as a directly contained column, not a subcolumn):
		Column<?> col = matcher.findColumnIn(columnSet);
		if(col != null)
		{
			path.push(col);
			return path;
		}
		else
		{	// Try to find the column as a subcolumn...
			for(Column<?> c : columnSet.getColumns(false))
				if(c instanceof ValueSetColumn)
				{
					path.push(c);
					if(ConstructPath(path, ((ValueSetColumn<?>) c).getColumnSet(), matcher) != null)
						return path;
					else
						path.pop();
				}
			return null;
		}
	}
	
	/**
	 * Helper interface to find a column in a Schema
	 */
	private static interface ColumnMatcher
	{
		
		/**
		 * @param columnSet the columnSet to look in (only looking at directly contained columns)
		 * @return the matching column, if there was one, or null otherwise
		 */
		public Column<?> findColumnIn(ColumnSet columnSet);
		
	}
	
	// DYNAMIC --------------------------------------------------------------------------
	private final Stack<Column<?>> columnStack;
	
	/**
	 * Note: in absence of a topLevelSchema we cannot check if this is a top level column
	 * 
	 * @param column the pointed-at column
	 */
	public ColumnPointer(C column)
	{
		this(Collections.<ValueSetColumn<?>> emptyList(), column);
	}
	
	/**
	 * Creates a ColumnPointer pointing to the given {@code column} following path through the given {@code parents}.
	 * 
	 * @param parents
	 * @param column the pointed-at column
	 */
	public ColumnPointer(List<ValueSetColumn<?>> parents, C column)
	{
		List<Column<?>> columns = new ArrayList<Column<?>>(parents.size() + 1);
		columns.addAll(parents);
		columns.add(column);
		
		// Build up stack and perform checks:
		columnStack = new Stack<Column<?>>();
		for(Column<?> col : columns)
		{
			if(col == null)
				throw new NullPointerException("Column cannot be null");
			if(!columnStack.isEmpty())
			{
				Column<?> prevCol = columnStack.peek(); 
				if(!(prevCol instanceof ValueSetColumn))
					throw new IllegalArgumentException("Column \"" + prevCol.getName() + "\" is expected to be a ValueSetColumn but it is not.");
				if(!((ValueSetColumn<?>) prevCol).getColumnSet().containsColumn(col))
					throw new IllegalArgumentException("Column \"" + col.getName() + "\" is not a subcolumn of ValueSetColumn \"" + prevCol.getName() + "\".");
			}
			columnStack.push(col);
		}
	}
	
	/**
	 * @param topLevelSchema
	 * @param column the pointed-at column
	 */
	public ColumnPointer(Schema topLevelSchema, C column)
	{
		this(topLevelSchema, column, false);
	}
	
	/**
	 * @param topLevelSchema
	 * @param column the pointed-at column
	 * @param allowEquivalent whether to allow pointing to a column equivalent to the given one (reachable from the topLevelSchema), or only the exact given column
	 */
	public ColumnPointer(Schema topLevelSchema, C column, boolean allowEquivalent)
	{
		columnStack = ConstructPathTo(topLevelSchema, column, allowEquivalent);
	}
	
	/**
	 * For internal use only.
	 * 
	 * @param columnStack
	 */
	private ColumnPointer(Stack<Column<?>> columnStack)
	{
		this.columnStack = columnStack;
	}
	
	/**
	 * Returns a (sub)valueSet of the given valueSet, or possibly itself, where the schema of the former contains the column this ColumnPointer points to.
	 * 
	 * @param topLevelVS
	 * @param create whether or not to create a new subValueSet if there is none
	 * @return the (sub)record or null if there was none and creation was disabled
	 * @throws IllegalArgumentException when no path could be constructed from the schema of the given record to the column pointed at by this ColumnPointer
	 */
	public ValueSet<?> getValueSet(ValueSet<?> topLevelVS, boolean create) throws IllegalArgumentException
	{
		// Get path:
		Stack<Column<?>> path = getPathFrom(topLevelVS.getColumnSet());
		
		// Get the right (sub)record:
		ValueSet<?> vs = topLevelVS;
		for(Column<?> col : path)
			if(col instanceof ValueSetColumn && col != path.peek())
			{
				ValueSetColumn<?> recCol = ((ValueSetColumn<?>) col);
				if(!recCol.isValueSet(vs))
				{
					if(create)
						recCol.storeObject(vs, recCol.getNewRecord());
					else
						return null;
				}
				vs = recCol.retrieveValue(vs);
			}
		
		// Return the (sub)record:
		return vs;
	}
	
	/**
	 * Get the value stored in the pointed-at column for the given top-Level ValueSet.
	 * Note: value is returned as an Object.
	 * 
	 * @param topLevelVS
	 * @return
	 */
	public Object retrieveValue(ValueSet<?> topLevelVS)
	{
		ValueSet<?> subVS = getValueSet(topLevelVS, false);
		if(subVS != null)
			return getColumn().retrieveValue(subVS);
		else
			return null;
	}
	
	/**
	 * @return whether or not the ColumnPointer points to a top-level (root) column, i.e. one that is directly contained in a/the schema
	 */
	public boolean isTopLevelColumn()
	{
		return columnStack.size() == 1; 
	}
	
	/**
	 * @return whether or not the ColumnPointer points to a column which is a child of a composite column
	 */
	public boolean isSubColumn()
	{
		return columnStack.size() > 1;
	}
	
	/**
	 * @return the column this ColumnPointer points to
	 */
	@SuppressWarnings("unchecked")
	public C getColumn()
	{
		if(columnStack.isEmpty())
			return null;
		return (C) columnStack.peek();
	}
	
	/**
	 * @return a new ColumnPointer pointing to the parent of the column to which this ColumnPoiter points, or null if that column was already top level
	 */
	public ColumnPointer<ValueSetColumn<?>> getParentPointer()
	{
		if(isSubColumn())
		{
			Stack<Column<?>> parentStack = new Stack<Column<?>>();
			for(Column<?> col : columnStack) // (iterates from bottom to top)
				if(parentStack.size() < columnStack.size() - 1)
					parentStack.push(col);
			return new ColumnPointer<ValueSetColumn<?>>(parentStack);
		}
		else
			return null;
	}
	
	/**
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown!)
	 */
	public String getQualifiedColumnName()
	{
		return getQualifiedColumnName(null, ValueSetColumn.QUALIFIED_NAME_SEPARATOR);
	}

	/**
	 * @param separator to put between column names in qualified name
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown!)
	 */
	public String getQualifiedColumnName(char separator)
	{
		return getQualifiedColumnName(null, separator);
	}
	
	/**
	 * @param topLevelSchema
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown and no non-null topLevelSchema is given)
	 * @throws IllegalArgumentException when no path could be constructed from the given schema to the column pointed at by this ColumnPointer
	 */
	public String getQualifiedColumnName(Schema topLevelSchema) throws IllegalArgumentException
	{
		return getQualifiedColumnName(topLevelSchema, ValueSetColumn.QUALIFIED_NAME_SEPARATOR);
	}
	
	/**
	 * @param topLevelSchema
	 * @param separator to put between column names in qualified name
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown and no non-null topLevelSchema is given)
	 * @throws IllegalArgumentException when no path could be constructed from the given schema to the column pointed at by this ColumnPointer
	 */
	public String getQualifiedColumnName(Schema topLevelSchema, char separator) throws IllegalArgumentException
	{
		// Get path:
		Stack<Column<?>> path = getPathFrom(topLevelSchema);
		
		TransactionalStringBuilder bldr = new TransactionalStringBuilder(separator);
		for(Column<?> col : path)
			bldr.append(col.getName());
		return bldr.toString();
	}
	
	/**
	 * @return a Comparator to compare Records in terms of values in the pointed-at {@link ComparableColumn} 
	 * @throws ClassCastException if the pointed-at column is not a {@link ComparableColumn}
	 */
	public Comparator<Record> getComparator() throws ClassCastException
	{
		Column<?> col = getColumn();
		if(col == null)
			return null;
		if(col instanceof ComparableColumn)
		{
			final ComparableColumn<?> compCol = (ComparableColumn<?>) col;
			return new Comparator<Record>()
			{
				
				@Override
				public int compare(Record lhs, Record rhs)
				{	// Compare (sub)records:
					return compCol.compare(getValueSet(lhs, false), getValueSet(rhs, false));
				}
			};
		}
		else
			throw new IllegalArgumentException("ColumnPointer does not point to a ComparatorColumn");
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof ColumnPointer)
		{
			ColumnPointer<?> that = (ColumnPointer<?>) obj;
			return columnStack.equals(that.columnStack);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		return columnStack.hashCode();
	}

	/**
	 * Returns a complete path leading from the given top-level ColumnSet to the column this ColumnPointer points to.
	 * This can be the columnStack if it is complete, or a newly constructed path if it is not.
	 * 
	 * @param topLevelCS
	 * @return a path to the pointed-at column
	 * @throws IllegalArgumentException when no path could be constructed
	 */
	private Stack<Column<?>> getPathFrom(ColumnSet topLevelCS) throws IllegalArgumentException
	{
		// Use columnStack as default path (will be tested for completeness below):
		Stack<Column<?>> path = columnStack;
		
		// Check if we already have a complete path to the pointed-at column starting from the top-level schema (if one was given):
		if(topLevelCS != null && !topLevelCS.containsEquivalentColumn(path.firstElement())) // Either the columnStack is missing parent columns, or this record is from another schema...
			path = ConstructPathTo(topLevelCS, getColumn(), true); // Try to construct a path to the pointed-at column or an equivalent one
		
		return path;
	}
	
	@Override
	public String toString()
	{
		return columnStack.toString();
	}
	
}
