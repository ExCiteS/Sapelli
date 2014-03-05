package uk.ac.ucl.excites.sapelli.storage.model;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;

/**
 * A Schema holds a set of ordered {@link Column}s
 * 
 * @author mstevens
 */
@SuppressWarnings("rawtypes")
public class Schema
{

	// Statics------------------------------------------------------------
	static public final int UNKNOWN_COLUMN_POSITION = -1;
	
	// Identification:
	static public final int SCHEMA_USAGE_ID_SIZE = 32; //bits
	static public final IntegerRangeMapping SCHEMA_USAGE_ID_FIELD = IntegerRangeMapping.ForSize(0, SCHEMA_USAGE_ID_SIZE); // unsigned(!) 32 bit integer
	static public final int SCHEMA_USAGE_SUB_ID_SIZE = 4; //bits
	static public final IntegerRangeMapping SCHEMA_USAGE_SUB_ID_FIELD = IntegerRangeMapping.ForSize(0, SCHEMA_USAGE_SUB_ID_SIZE); // unsigned(!) 4 bit integer
	static public final int DEFAULT_USAGE_SUB_ID = 0;
	static public final String ATTRIBUTE_USAGE_ID = "usageID";
	static public final String ATTRIBUTE_USAGE_SUB_ID = "usageSubID";
	static public final String ATTRIBUTE_SCHEMA_NAME = "schemaName";
	
	// v1.x-style identification (for backwards compatibility only):
	//	Note: schemaID & schemaVersion are no longer stored in a Schema instance, instead a 1.x Project instance holds them (Project#id = schemaID & Project#schemaVersion = schemaVersion) 
	static public final int V1X_SCHEMA_ID_SIZE = 24; //bits
	static public final int V1X_SCHEMA_VERSION_SIZE = 8; //bits
	static public final IntegerRangeMapping V1X_SCHEMA_VERSION_FIELD = IntegerRangeMapping.ForSize(0, V1X_SCHEMA_VERSION_SIZE);
	static public final int V1X_DEFAULT_SCHEMA_VERSION = 0;
	// 	Note the XML attributes below have inconsistent naming (for everything else we've been using CamelCase instead of dashes), won't fix because no longer used in v2.x
	static public final String V1X_ATTRIBUTE_SCHEMA_ID = "schema-id";
	static public final String V1X_ATTRIBUTE_SCHEMA_VERSION = "schema-version";
	
	// Dynamics-----------------------------------------------------------
	private long usageID;
	private int usageSubID;
	private String name;

	private List<Column> columns;
	private Map<String, Integer> columnNameToPosition;
	private List<Index> indexes;
	private Index primaryKey;

	private boolean sealed = false;
	
	public Schema(long usageID)
	{
		this(usageID, DEFAULT_USAGE_SUB_ID, null);
	}

	public Schema(long usageID, String name)
	{
		this(usageID, DEFAULT_USAGE_SUB_ID, name);
	}

	public Schema(long usageID, int usageSubID)
	{
		this(usageID, usageSubID, null);
	}

	public Schema(long usageID, int usageSubID, String name)
	{
		if(SCHEMA_USAGE_ID_FIELD.fits(usageID))
			this.usageID = usageID;
		else
			throw new IllegalArgumentException("Invalid schema usageID value (" + usageID + "), valid values are " + SCHEMA_USAGE_ID_FIELD.getLogicalRangeString() + ".");
		if(SCHEMA_USAGE_SUB_ID_FIELD.fits(usageSubID))
			this.usageSubID = usageSubID;
		else
			throw new IllegalArgumentException("Invalid schema usageSubID value (" + usageSubID + "), valid values are " + SCHEMA_USAGE_SUB_ID_FIELD.getLogicalRangeString() + ".");
		this.name = (name == null || name.isEmpty() ? "Schema_UID:" + usageID + "_USID:" + usageSubID : name);
		columnNameToPosition = new LinkedHashMap<String, Integer>();
		columns = new ArrayList<Column>();
		indexes = new ArrayList<Index>();
	}

	public void addColumns(List<Column<?>> columns)
	{
		for(Column c : columns)
			addColumn(c);
	}
	
	public void addColumn(Column column)
	{
		if(!sealed)
		{
			columnNameToPosition.put(column.getName(), columns.size());
			columns.add(column);
			column.setSchema(this);
		}
		else
			throw new IllegalStateException("Cannot extend a sealed schema!");
	}

	public Column getColumn(int position)
	{
		try
		{
			return columns.get(position);
		}
		catch(IndexOutOfBoundsException iobe)
		{
			return null;
		}
	}

	/**
	 * @param name
	 * @return	the {@link Column} instance with this name, or {@code null} if the Schema contains no such column
	 */
	public Column getColumn(String name)
	{
		Integer idx = columnNameToPosition.get(name);
		if(idx == null)
			return null;
		return columns.get(idx);
	}

	public List<Column> getColumns()
	{
		return columns;
	}

	/**
	 * @param name
	 * @return	the position of the {@link Column} instance with this name, or {@link #UNKNOWN_COLUMN_POSITION} if the Schema contains no such column
	 */
	public int getColumnPosition(String name)
	{
		Integer idx = columnNameToPosition.get(name);
		if(idx == null)
			return UNKNOWN_COLUMN_POSITION;
		return idx.intValue();
	}

	/**
	 * @param column
	 * @return	the position of the given {@link Column} instance within this Schema, or {@link #UNKNOWN_COLUMN_POSITION} if the Schema contains no such column
	 */
	public int getColumnPosition(Column column)
	{
		return getColumnPosition(column.getName());
	}
	
	/**
	 * 
	 * @param column
	 * @return	whether or not this Schema contains the given Column
	 */
	public boolean containsColumn(Column column)
	{
		Column myColumn = getColumn(column.getName());
		return myColumn != null && myColumn.equals(column);
	}
	
	/**
	 * Add an {@link Index} to the Schema, which may or may not be used as the primary key.
	 * In case it is to be used as the primary key the index needs to be unique and should consist only of non-optional (i.e. non-nullable) columns.
	 * 
	 * Note: for now we allow indexes to be added and a primary key to be set after the schema has been sealed. However this may change in the future.
	 * 
	 * @param index
	 * @param useAsPrimaryKey
	 */
	public void addIndex(Index index, boolean useAsPrimaryKey)
	{
		if(index == null)
			throw new IllegalArgumentException("Index cannot be null!");
		// Check if the indexed columns are columns of this Schema instance:
		for(Column idxCol : index.getColumns())
			if(!containsColumn(idxCol))
				throw new IllegalArgumentException("Indexed column '" + idxCol.getName() + "' does not belong to this Schema. Indexed columns need to be added to the Schema before Indexes are added.");
		if(useAsPrimaryKey)
		{
			if(primaryKey != null)
				throw new IllegalStateException("This Schema already has a primary key (there can be only 1)!");
			if(!index.isUnique())
				throw new IllegalArgumentException("An Index needs to be unique to serve as the primary key!");
			for(Column idxCol : index.getColumns())
				if(idxCol.isOptional())
					throw new IllegalArgumentException("An primary key index cannot contain optional (i.e. nullable) columns!");
			primaryKey = index; // set the index as primary key
		}
		indexes.add(index); // add to the indexes
	}

	/**
	 * @return the sealed
	 */
	public boolean isSealed()
	{
		return sealed;
	}

	/**
	 * seals the schema, after which records can be created based on it, but no more columns can be added
	 */
	public void seal()
	{
		this.sealed = true;
	}

	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}
	
	/**
	 * @return the usageID
	 */
	public long getUsageID()
	{
		return usageID;
	}

	/**
	 * @return the usageSubID
	 */
	public int getUsageSubID()
	{
		return usageSubID;
	}

	public int getNumberOfColumns()
	{
		return columns.size();
	}

	/**
	 * @return whether or not the size taken up by binary stored records of this schema varies at run-time (i.e. depending on input)
	 */
	public boolean isVariableSize()
	{
		for(Column<?> c : columns)
			if(c.isVariableSize())
				return true;
		return false;
	}

	/**
	 * Returns the number of bits a record of this schema takes up when written to a binary representation. In case of a variable size the maximum effective
	 * size is returned.
	 * 
	 * @return
	 */
	public int getSize()
	{
		return getMaximumSize();
	}

	/**
	 * Returns the minimum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @return
	 */
	public int getMinimumSize()
	{
		return getMinimumSize(null);
	}
	
	/**
	 * Returns the minimum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @param columns to ignore the total
	 * @return
	 */
	public int getMinimumSize(Set<Column<?>> skipColumns)
	{
		int total = 0;
		for(Column<?> c : columns)
			if(skipColumns == null || !skipColumns.contains(c))
				total += c.getMinimumSize();
		return total;
	}
	
	/**
	 * Returns the maximum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @return
	 */
	public int getMaximumSize()
	{
		return getMaximumSize(null);
	}

	/**
	 * Returns the maximum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @param columns to ignore the total
	 * @return
	 */
	public int getMaximumSize(Set<Column<?>> skipColumns)
	{
		int total = 0;
		for(Column<?> c : columns)
			if(skipColumns == null || !skipColumns.contains(c))
				total += c.getMaximumSize();
		return total;
	}
	
	/**
	 * Check for equality based on schema ID & version (nothing else)
	 * 
	 * @param obj object to compare this one with
	 * @return whether or not the given Object is a Schema with the same ID & version as this one
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		return equals(obj, false, false); // check only usageID & usageSubID by default
	}

	/**
	 * Check if the provided object is an identical/equivalent Schema. The usageID & usageSubID are always checked, names and columns are optionally checked, descriptions are ignored. 
	 * 
	 * @param obj object to compare this one with
	 * @param checkNames whether or not to compare the names of the schemas and (if checkColumns is true) those of their columns
	 * @param checkColumns whether or not to compare columns (types, sizes, etc., and names if checkNames is true)
	 * @return whether or not the given Object is an identical/equivalent Schema
	 */
	public boolean equals(Object obj, boolean checkNames, boolean checkColumns)
	{
		if(obj instanceof Schema)
		{
			Schema other = (Schema) obj;
			// Usage:
			boolean usageMatch = (this.usageID == other.usageID) && (this.usageSubID == other.usageSubID);
			if(!(checkNames || checkColumns) || !usageMatch)
				return usageMatch;
			// Name:
			if(checkNames && !this.name.equals(other.name))
				return false;
			// Columns:
			if(checkColumns)
			{
				// Check number of columns:
				if(columns.size() != other.columns.size())
					return false;
				// Compare columns:
				Iterator<Column> myCols = columns.iterator();
				Iterator<Column> otherCols = other.columns.iterator();
				while(myCols.hasNext() /* && otherCols.hasNext() */)
					if(!myCols.next().equals(otherCols.next(), checkNames))
						return false;
			}
			return true;
		}
		else
			return false;
	}
	
	@Override
	public String toString()
	{
		return "Schema " + name;
	}
	
	public String getSpecification()
	{
		StringBuffer bff = new StringBuffer();
		bff.append(toString() + ":");
		for(Column<?> c : columns)
			bff.append("\n\t- " + c.toString());
		return bff.toString();
	}
	
	public String toCSVHeader(String separator)
	{
		StringBuffer bff = new StringBuffer();
		for(Column<?> c : columns)
			bff.append(c.getName());
		return bff.toString();
	}

}