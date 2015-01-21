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
				if(!((RecordColumn<?>) prevCol).getSchema().containsColumn(col, true))
					throw new IllegalArgumentException("Column \"" + col.getName() + "\" is not a subcolumn of RecordColumn \"" + prevCol.getName() + "\".");
			}
			columnStack.push(col);
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
	 * @param topLevelSchema
	 * @param column
	 */
	public ColumnPointer(Schema topLevelSchema, Column<?> column)
	{
		columnStack = constructPathTo(topLevelSchema, column);
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
	
	private boolean findColumn(Stack<Column<?>> stack, Schema schema, String columnName)
	{
		if(schema.containsColumn(columnName, true))
		{
			stack.push(schema.getColumn(columnName, true));
			return true;
		}
		else
		{
			for(Column<?> c : schema.getColumns(true))
				if(c instanceof RecordColumn)
				{
					stack.push(c);
					if(findColumn(stack, ((RecordColumn<?>) c).getSchema(), columnName))
						return true;
					else
						stack.pop();
				}
			return false;
		}
	}

	private Stack<Column<?>> constructPathTo(Schema topLevelSchema, Column<?> column)
	{
		Stack<Column<?>> path = constructPathTo(topLevelSchema, column.getName(), false);
		
		// Check if we got the right one:
		if(!column.equals(path.peek()))
			throw new IllegalArgumentException("Column \"" + column.getName() + "\" is not part of " + topLevelSchema.toString());
		
		return path;
	}
	
	private Stack<Column<?>> constructPathTo(Schema topLevelSchema, String columnName, boolean trySanitising)
	{
		// Find column & construct path to it:
		Stack<Column<?>> path = new Stack<Column<?>>();
		findColumn(path, topLevelSchema, columnName);
		
		if(path.isEmpty())
		{	
			if(trySanitising) // try again with sanitised name if allowed:
				return constructPathTo(topLevelSchema, Column.SanitiseName(columnName), false); // pass false to avoid endless sanitation loop ;-)
			throw new IllegalArgumentException("Column \"" + columnName + "\" not found in " + topLevelSchema.toString());
		}
		return path;
	}

	/**
	 * Returns a (sub)record of the given record (but possible itself), where the schema of the former contains the column this ColumnPointer points to 
	 * 
	 * @param topLevelRecord
	 * @param create
	 * @return
	 */
	public Record getRecord(Record topLevelRecord, boolean create)
	{
		Stack<Column<?>> path = columnStack;
		
		// Check if we have a complete path to the pointed-at column starting from the top-level schema:
		if(!topLevelRecord.getSchema().containsColumn(path.firstElement(), true)) // Either the columnStack is missing parent columns, or this record is from another schema...
			path = constructPathTo(topLevelRecord.getSchema(), getColumn()); // Try to construct a path to the pointed-at record
		
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
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public ColumnPointer getParentPointer()
	{
		if(isSubColumn())
		{
			Stack<Column<?>> parentStack = (Stack<Column<?>>) columnStack.clone();
			parentStack.pop();
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
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown and no non-null topLevelSchema is given)
	 */
	public String getQualifiedColumnName(Schema topLevelSchema)
	{
		return getQualifiedColumnName(topLevelSchema, RecordColumn.QUALIFIED_NAME_SEPARATOR);
	}
	
	/**
	 * @param separator to put between column names in qualified name
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown and no non-null topLevelSchema is given)
	 */
	public String getQualifiedColumnName(Schema topLevelSchema, char separator)
	{
		Stack<Column<?>> path = columnStack;
		
		// Check if we have a complete path to the pointed-at column starting from the top-level schema (if one was given):
		if(topLevelSchema != null && !topLevelSchema.containsColumn(path.firstElement(), true)) // Either the columnStack is missing parent columns, or this record is from another schema...
			path = constructPathTo(topLevelSchema, getColumn()); // Try to construct a path to the pointed-at record
		
		StringBuilder bldr = new StringBuilder();
		for(Column<?> col : path)
		{
			if(col != path.firstElement())
				bldr.append(separator);
			bldr.append(col.getName());				
		}
		return bldr.toString();
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
	
	public Comparator<Record> getComparator()
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
				{
					// Get sub records:
					lhs = getRecord(lhs, false);
					rhs = getRecord(rhs, false);
					
					// Compare:
					return lhs == null ?
							(rhs == null ? 0 : Integer.MIN_VALUE) :
							(rhs == null ? Integer.MAX_VALUE : compCol.compare(lhs, rhs));
				}
				
			};
		}
		else
			throw new IllegalArgumentException("ColumnPointer does not point to a ComparatorColumn");
	}

}
