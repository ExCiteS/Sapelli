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

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * Helper class to locate & point to (sub)Columns in Schemas and RecordColumns
 * 
 * @author mstevens
 */
public class ColumnPointer
{
	
	private final Stack<Column<?>> columnStack;
	
	/**
	 * Note: in absence of a topLevelSchema we cannot check if this is a top level column
	 * 
	 * @param column
	 */
	public ColumnPointer(Column<?> column)
	{
		this(new Column<?>[] { column });
	}
	
	/**
	 * If more than one column is passed all but the last one must be RecordColumns. The first
	 * column that is passed is assumed to be a top-level column but this cannot be checked in
	 * absence of a schema. Each subsequent column must be a part of the schema of the previous
	 * (Record)Column.
	 * 
	 * @param columns
	 */
	public ColumnPointer(Column<?>... columns)
	{
		this(Arrays.asList(columns));
	}
	
	/**
	 * If more than one column is passed all but the last one must be RecordColumns. The first
	 * column that is passed is assumed to be a top-level column but this cannot be checked in
	 * absence of a schema. Each subsequent column must be a part of the schema of the previous
	 * (Record)Column.
	 * 
	 * Note: in absence of a topLevelSchema we cannot check if the first column is a top level one
	 * 
	 * @param columns
	 */
	public ColumnPointer(List<Column<?>> columns)
	{
		columnStack = new Stack<Column<?>>();
		for(Column<?> col : columns)
		{
			if(col == null)
				throw new NullPointerException("Column cannot be null");
			if(!columnStack.isEmpty())
			{
				Column<?> prevCol = columnStack.peek(); 
				if(!(prevCol instanceof RecordColumn))
					throw new IllegalArgumentException("Column \"" + prevCol.getName() + "\" is expected to be a RecordColumn but it is not.");
				if(!((RecordColumn<?>) prevCol).getSchema().containsColumn(col))
					throw new IllegalArgumentException("Column \"" + col.getName() + "\" is not a subcolumn of RecordColumn \"" + prevCol.getName() + "\".");
			}
			columnStack.push(col);
		}
	}
	
	/**
	 * @param topLevelSchema
	 * @param column
	 */
	public ColumnPointer(Schema topLevelSchema, Column<?> column)
	{
		this(topLevelSchema, column, false);
	}
	
	/**
	 * @param topLevelSchema
	 * @param column
	 * @param allowEquivalent whether to allow pointing to a column equivalent to the given one (reachable from the topLevelSchema), or only the exact given column
	 */
	public ColumnPointer(Schema topLevelSchema, Column<?> column, boolean allowEquivalent)
	{
		columnStack = constructPathTo(topLevelSchema, column, allowEquivalent);
	}
	
	/**
	 * @param topLevelSchema
	 * @param columnName
	 */
	public ColumnPointer(Schema topLevelSchema, String columnName)
	{	
		// Null & empty check:
		if(columnName == null || columnName.isEmpty())
			throw new IllegalArgumentException("columnName cannot be null or empty");
		
		// Build up stack:
		if(columnName.indexOf(RecordColumn.QUALIFIED_NAME_SEPARATOR) == -1)
			columnStack = constructPathTo(topLevelSchema, columnName, true); // Find by columnName (recursive, depth-first search; columnName will be sanitised if not found as is)
		else
		{
			columnStack = new Stack<Column<?>>();
			// Find by qualified name:
			Schema schema = topLevelSchema;
			for(String colName : columnName.split("\\" + RecordColumn.QUALIFIED_NAME_SEPARATOR))
			{
				// Deal with previous (record)column:
				if(!columnStack.isEmpty())
					schema = ((RecordColumn<?>) columnStack.peek()).getSchema();
				// Deal with current column:
				Column<?> col = schema.getColumn(colName, true);
				// If not found...
				if(col == null)
					col = schema.getColumn(Column.SanitiseName(colName), true); // ... try again with sanitised name
				// If still not found...
				if(col == null)
					throw new IllegalArgumentException("Column \"" + columnName + "\" not found in " + topLevelSchema.toString());
				// Found:
				columnStack.push(col);
			}
		}
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param columnStack
	 */
	private ColumnPointer(Stack<Column<?>> columnStack)
	{
		this.columnStack = columnStack;
	}
	
	/**
	 * Returns a (sub)record of the given record, or possibly itself, where the schema of the former contains the column this ColumnPointer points to.
	 * 
	 * @param topLevelRecord
	 * @param create whether or not to create a new subrecord if there is none
	 * @return the (sub)record or null if there was none and creation was disabled
	 * @throws IllegalArgumentException when no path could be constructed from the schema of the given record to the column pointed at by this ColumnPointer
	 */
	public Record getRecord(Record topLevelRecord, boolean create) throws IllegalArgumentException
	{
		// Get path:
		Stack<Column<?>> path = getPathFrom(topLevelRecord.getSchema());
		
		// Get the right (sub)record:
		Record record = topLevelRecord;
		for(Column<?> col : path)
			if(col instanceof RecordColumn && col != path.peek())
			{
				RecordColumn<?> recCol = ((RecordColumn<?>) col);
				if(!recCol.isValueSet(record))
				{
					if(create)
						recCol.storeObject(record, recCol.getNewRecord());
					else
						return null;
				}
				record = recCol.retrieveValue(record);
			}
		
		// Return the (sub)record:
		return record;
	}
	
	/**
	 * Get the value stored in the pointed-at column for the given topLevelRecord.
	 * Note: value is returned as an Object.
	 * 
	 * @param topLevelRecord
	 * @return
	 */
	public Object retrieveValue(Record topLevelRecord)
	{
		Record subRecord = getRecord(topLevelRecord, false);
		if(subRecord != null)
			return getColumn().retrieveValue(subRecord);
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
	public Column<?> getColumn()
	{
		if(columnStack.isEmpty())
			return null;
		return columnStack.peek();
	}
	
	/**
	 * @return a new ColumnPointer pointing to the parent of the column to which this ColumnPoiter points, or null if that column was already top level
	 */
	public ColumnPointer getParentPointer()
	{
		if(isSubColumn())
		{
			Stack<Column<?>> parentStack = new Stack<Column<?>>();
			for(Column<?> col : columnStack) // (iterates from bottom to top)
				if(parentStack.size() < columnStack.size() - 1)
					parentStack.push(col);
			return new ColumnPointer(parentStack);
		}
		else
			return null;
	}
	
	/**
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown!)
	 */
	public String getQualifiedColumnName()
	{
		return getQualifiedColumnName(null, RecordColumn.QUALIFIED_NAME_SEPARATOR);
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
		return getQualifiedColumnName(topLevelSchema, RecordColumn.QUALIFIED_NAME_SEPARATOR);
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
					return compCol.compare(getRecord(lhs, false), getRecord(rhs, false));
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
			ColumnPointer that = (ColumnPointer) obj;
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
	 * Returns a complete path leading from the given topLevelSchema to the column this ColumnPointer points to.
	 * This can be the columnStack if it is complete, or a newly constructed path if it is not.
	 * 
	 * @param topLevelSchema
	 * @return a path to the pointed-at column
	 * @throws IllegalArgumentException when no path could be constructed
	 */
	private Stack<Column<?>> getPathFrom(Schema topLevelSchema) throws IllegalArgumentException
	{
		// Use columnStack as default path (will be tested for completeness below):
		Stack<Column<?>> path = columnStack;
		
		// Check if we already have a complete path to the pointed-at column starting from the top-level schema (if one was given):
		if(topLevelSchema != null && !topLevelSchema.containsEquivalentColumn(path.firstElement())) // Either the columnStack is missing parent columns, or this record is from another schema...
			path = constructPathTo(topLevelSchema, getColumn(), true); // Try to construct a path to the pointed-at column or an equivalent one
		
		return path;
	}
	
	/**
	 * Constructs a new path to the given column, or an equivalent one if allowEquivalent is true.
	 * 
	 * @param topLevelSchema
	 * @param column
	 * @param allowEquivalent whether to allow pointing to a column equivalent to the given one (reachable from the topLevelSchema), or only the exact given column
	 * @return a path to the column
	 * @throws IllegalArgumentException when no path could be constructed
	 */
	private Stack<Column<?>> constructPathTo(Schema topLevelSchema, final Column<?> column, boolean allowEquivalent) throws IllegalArgumentException
	{
		// Find column & construct path to it:
		Stack<Column<?>> path = constructPath(
			new Stack<Column<?>>(),
			topLevelSchema,
			allowEquivalent ?
				new ColumnMatcher()
				{
					@Override
					public Column<?> findColumnIn(Schema schema)
					{
						return schema.getEquivalentColumn(column);
					}
				} :
				new ColumnMatcher()
				{
					@Override
					public Column<?> findColumnIn(Schema schema)
					{
						if(schema.containsColumn(column))
							return column;
						else
							return null;
					}
				});
		// Check if we have a valid path:
		if(path == null || /*just in case*/ path.isEmpty())
			throw new IllegalArgumentException("Column \"" + column.getName() + "\" is not part of " + topLevelSchema.toString());
		return path;
	}
	
	/**
	 * Constructs a new path to a column identified only by a name.
	 * 
	 * @param topLevelSchema
	 * @param columnName name of the column to construct a path to
	 * @param trySanitising
	 * @return a path to the column
	 * @throws IllegalArgumentException when no path could be constructed
	 */
	private Stack<Column<?>> constructPathTo(Schema topLevelSchema, final String columnName, boolean trySanitising) throws IllegalArgumentException
	{
		// Find column & construct path to it:
		Stack<Column<?>> path = constructPath(new Stack<Column<?>>(), topLevelSchema, new ColumnMatcher()
		{
			@Override
			public Column<?> findColumnIn(Schema schema)
			{
				return schema.getColumn(columnName, true); // we allow virtual columns
			}
		});
		// Check if we have a valid path:
		if(path == null || /*just in case*/ path.isEmpty())
		{
			if(trySanitising) // try again with sanitised name if allowed:
				try
				{
					return constructPathTo(topLevelSchema, Column.SanitiseName(columnName), false); // pass false to avoid endless sanitation loop ;-)
				}
				catch(IllegalArgumentException iae) { /* exception is thrown below */ }
			throw new IllegalArgumentException("Column \"" + columnName + "\"" + (trySanitising ? " (or \"" + Column.SanitiseName(columnName) + "\")" : "") + " not found in " + topLevelSchema.toString());
		}
		return path;
	}
	
	/**
	 * Helper interface to find a column in a Schema
	 */
	private static interface ColumnMatcher
	{
		
		/**
		 * @param schema the schema to look in (only looking at directly contained columns)
		 * @return the matching column, if there was one, or null otherwise
		 */
		public Column<?> findColumnIn(Schema schema);
		
	}
	
	/**
	 * Recursive method to construct a path to a column identified using the given ColumnMatcher
	 * Uses depth-first traversal.
	 *   
	 * @param path Stack to build the path on
	 * @param schema
	 * @param matcher
	 * @return a complete path to the column matched by the matcher, or null if it could not be found
	 */
	private Stack<Column<?>> constructPath(Stack<Column<?>> path, Schema schema, ColumnMatcher matcher)
	{
		// Try to find the column in the current schema (as a directly contained column, not a subcolumn):
		Column<?> col = matcher.findColumnIn(schema);
		if(col != null)
		{
			path.push(col);
			return path;
		}
		else
		{	// Try to find the column as a subcolumn...
			for(Column<?> c : schema.getColumns(false))
				if(c instanceof RecordColumn)
				{
					path.push(c);
					if(constructPath(path, ((RecordColumn<?>) c).getSchema(), matcher) != null)
						return path;
					else
						path.pop();
				}
			return null;
		}
	}

}
