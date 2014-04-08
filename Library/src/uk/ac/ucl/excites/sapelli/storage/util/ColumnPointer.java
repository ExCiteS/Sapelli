/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.util;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparatorColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * Helper class to locate & point to (sub)Columns in Schemas and RecordColumns
 * 
 * @author mstevens
 */
public class ColumnPointer implements Comparator<Record>
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
	
	public ColumnPointer(Schema topLevelSchema, Column<?> column)
	{
		columnStack = constructPathTo(topLevelSchema, column);
	}
	
	public ColumnPointer(Schema topLevelSchema, String columnName)
	{	
		// Null & empty check:
		if(columnName == null || columnName.isEmpty())
			throw new IllegalArgumentException("columnName cannot be null or empty");
		
		// Build up stack:
		if(columnName.indexOf(RecordColumn.QUALIFIED_NAME_SEPARATOR) == -1)
			columnStack = constructPathTo(topLevelSchema, columnName); // Find by columnName (recursive, depth-first search)
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
				if(col != null)
					columnStack.push(col);
				else
					throw new IllegalArgumentException("Column \"" + columnName + "\" not found in " + topLevelSchema.toString());
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
		Stack<Column<?>> path = constructPathTo(topLevelSchema, column.getName());
		
		// Check if we got the right one:
		if(!column.equals(path.peek()))
			throw new IllegalArgumentException("Column \"" + column.getName() + "\" is not part of " + topLevelSchema.toString());
		
		return path;
	}
	
	private Stack<Column<?>> constructPathTo(Schema topLevelSchema, String columnName)
	{
		Stack<Column<?>> path = new Stack<Column<?>>();
		findColumn(path, topLevelSchema, columnName);
		if(path.isEmpty())
			throw new IllegalArgumentException("Column \"" + columnName + "\" not found in " + topLevelSchema.toString());
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
	 * @return the column this ColumnPointer points to
	 */
	public Column<?> getColumn()
	{
		if(columnStack.isEmpty())
			return null;
		return columnStack.peek();
	}
	
	/**
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown!)
	 */
	public String getQualifiedColumnName()
	{
		return getQualifiedColumnName(null);
	}
	
	/**
	 * @return the qualified name of the column this ColumnPointer points to (can be incomplete if parent column(s) are unknown and no non-null topLevelSchema is given)
	 */
	public String getQualifiedColumnName(Schema topLevelSchema)
	{
		Stack<Column<?>> path = columnStack;
		
		// Check if we have a complete path to the pointed-at column starting from the top-level schema (if one was given):
		if(topLevelSchema != null && !topLevelSchema.containsColumn(path.firstElement(), true)) // Either the columnStack is missing parent columns, or this record is from another schema...
			path = constructPathTo(topLevelSchema, getColumn()); // Try to construct a path to the pointed-at record
		
		StringBuilder bldr = new StringBuilder();
		for(Column<?> col : path)
		{
			if(col != path.firstElement())
				bldr.append(RecordColumn.QUALIFIED_NAME_SEPARATOR);
			bldr.append(col.getName());				
		}
		return bldr.toString();
	}

	@SuppressWarnings("rawtypes")
	@Override
	public int compare(Record lhs, Record rhs)
	{
		if(!(getColumn() instanceof ComparatorColumn))
			throw new IllegalStateException("This ColumnPointer does not point to a ComparatorColumn");
		
		// Get sub records:
		lhs = getRecord(lhs, false);
		rhs = getRecord(rhs, false);
		
		// Compare:
		return lhs == null ?
				(rhs == null ? 0 : Integer.MIN_VALUE) :
				(rhs == null ? Integer.MAX_VALUE : ((ComparatorColumn) getColumn()).compare(lhs, rhs));		
	}

}
