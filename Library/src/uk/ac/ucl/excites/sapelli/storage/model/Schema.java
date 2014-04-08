package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A Schema holds a set of ordered {@link Column}s
 * 
 * @author mstevens
 */
@SuppressWarnings("rawtypes")
public class Schema implements Serializable
{

	private static final long serialVersionUID = 2L;

	// Statics------------------------------------------------------------
	static protected final int UNKNOWN_COLUMN_POSITION = -1;
	
	// Identification:
	static public final int SCHEMA_ID_SIZE = 36; //bits
	static public final IntegerRangeMapping SCHEMA_ID_FIELD = IntegerRangeMapping.ForSize(0, SCHEMA_ID_SIZE); // unsigned(!) 36 bit integer
	
	static public final String ATTRIBUTE_SCHEMA_ID = "schemaID";
	static public final String ATTRIBUTE_SCHEMA_NAME = "schemaName";
	static public enum ReservedIDs
	{
		INDEX_SCHEMA,		/* 0 */
		LOCATION_SCHEMA,	/* 1 */
		ORIENTATION_SCHEMA	/* 2 */
		// more later?
	}
	
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
	protected final long id;
	protected final String name;

	private final List<Column> columns; // only contains non-virtual columns
	private final Map<String, Integer> columnNameToPosition; // only contans non-virtual columns
	private final Map<Column, List<Column>> columnToVirtualColumns;
	private final Map<String, Column> virtualColumnsByName;
	private final List<Index> indexes;
	private Index primaryKey;

	private boolean sealed = false;
	
	public Schema(long id, String name)
	{
		if(SCHEMA_ID_FIELD.fits(id))
			this.id = id;
		else
			throw new IllegalArgumentException("Invalid schema ID value (" + id + "), valid values are " + SCHEMA_ID_FIELD.getLogicalRangeString() + ".");
		this.name = (name == null || name.isEmpty() ? "Schema_ID" + id : name);
		columnNameToPosition = new LinkedHashMap<String, Integer>();
		columns = new ArrayList<Column>();
		virtualColumnsByName = new HashMap<String, Column>();
		indexes = new ArrayList<Index>();
		columnToVirtualColumns = new HashMap<Column, List<Column>>();
	}

	public void addColumns(List<Column<?>> columns)
	{
		for(Column c : columns)
			addColumn(c);
	}
	
	public void addColumn(Column column)
	{
		if(sealed)
			throw new IllegalStateException("Cannot extend a sealed schema!");
		if(containsColumn(column, true))
			throw new IllegalArgumentException("The schema already contains a column with name \"" + column.getName() + "\"!");
		if(!column.isVirtual())
		{	// Add the column:
			columnNameToPosition.put(column.getName(), columns.size());
			columns.add(column);			
		}
		else
		{	// Deal with virtual column:
			Column sourceColumn = column.getSourceColumn(); 
			if(!containsColumn(sourceColumn, false))
				throw new IllegalArgumentException("The schema does not contain the source column of the given virtual column.");
			if(!columnToVirtualColumns.containsKey(sourceColumn))
				columnToVirtualColumns.put(sourceColumn, new ArrayList<Column>());
			columnToVirtualColumns.get(sourceColumn).add(column);
		}
	}
	
	/**
	 * Add an {@link Index} to the Schema, which may or may not be used as the primary key.
	 * In case it is to be used as the primary key the index needs to be unique and should consist only of non-optional (i.e. non-nullable) columns.
	 * 
	 * Note: adding a primary key index is not allowed after the Schema has been sealed, adding normal indexes is allowed.
	 * 
	 * @param index
	 * @param useAsPrimaryKey
	 */
	public void addIndex(Index index, boolean useAsPrimaryKey)
	{
		if(sealed && useAsPrimaryKey)
			throw new IllegalStateException("Cannot set the primary key of a sealed schema (adding normal indexes is allowed)!");
		if(index == null)
			throw new IllegalArgumentException("Index cannot be null!");
		// Check if the indexed columns are columns of this Schema instance:
		for(Column idxCol : index.getColumns(false))
			if(!containsColumn(idxCol, false))
				throw new IllegalArgumentException("Indexed column '" + idxCol.getName() + "' does not belong to this Schema. Indexed columns need to be added to the Schema before Indexes are added.");
		if(useAsPrimaryKey)
		{
			if(primaryKey != null)
				throw new IllegalStateException("This Schema already has a primary key (there can be only 1)!");
			if(!index.isUnique())
				throw new IllegalArgumentException("An Index needs to be unique to serve as the primary key!");
			for(Column idxCol : index.getColumns(false))
				if(idxCol.isOptional())
					throw new IllegalArgumentException("An primary key index cannot contain optional (i.e. nullable) columns!");
			primaryKey = index; // set the index as primary key
		}
		indexes.add(index); // add to the indexes
	}

	/**
	 * @param name
	 * @param checkVirtual whether or not to look in the schema's virtual columns
	 * @return the {@link Column} instance with this name, or {@code null} if the Schema contains no such column
	 */
	public Column getColumn(String name, boolean checkVirtual)
	{
		Integer pos = columnNameToPosition.get(name);
		if(pos == null)
			return checkVirtual ? virtualColumnsByName.get(name) : null;
		return columns.get(pos);
	}

	public List<Column> getColumns(boolean includeVirtual)
	{
		if(includeVirtual)
		{
			List<Column> allCols = new ArrayList<Column>();
			for(Column nonVirtualCol : columns)
			{
				allCols.add(nonVirtualCol);
				CollectionUtils.addAllIgnoreNull(allCols, columnToVirtualColumns.get(nonVirtualCol));
			}
			return allCols;
		}
		return columns;
	}
	
	public Collection<Column> getVirtualColumns()
	{
		return virtualColumnsByName.values();
	}

	/**
	 * Returns the position of a given non-virtual column.
	 * Null is returned if the schema doesn't contain the column at all, but also if the column is virtual (even if the source is contained by the schema).
	 * 
	 * @param column a non-virtual column
	 * @return	the position of the given {@link Column} instance within this Schema, or {@link #UNKNOWN_COLUMN_POSITION} if the Schema contains no such column
	 */
	protected int getColumnPosition(Column column)
	{
		Integer idx = columnNameToPosition.get(column.getName());
		if(idx == null)
			return UNKNOWN_COLUMN_POSITION;
		return idx.intValue();
	}
	
	/**
	 * @param name the name of a column
	 * @param checkVirtual whether or not to look in the schema's virtual columns
	 * @return
	 */
	public boolean containsColumn(String name, boolean checkVirtual)
	{
		return columnNameToPosition.containsKey(name) || (checkVirtual && virtualColumnsByName.containsKey(name));
	}
	
	/**
	 * 
	 * @param column
	 * @param checkVirtual whether or not to look in the schema's virtual columns
	 * @return	whether or not this Schema contains the given Column or an exact equivalent of it
	 */
	public boolean containsColumn(Column column, boolean checkVirtual)
	{
		Column myColumn = getColumn(column.getName(), checkVirtual);
		return myColumn != null && myColumn.equals(column);
	}

	/**
	 * @return the indexes
	 */
	public List<Index> getIndexes()
	{
		return indexes;
	}

	/**
	 * @return the primaryKey
	 */
	public Index getPrimaryKey()
	{
		return primaryKey;
	}
	
	/**
	 * @return	whether or not the Schema has a primary key
	 */
	public boolean hasPrimaryKey()
	{
		return primaryKey != null;
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
	 * @return the id
	 */
	public long getID()
	{
		return id;
	}

	public int getNumberOfColumns()
	{
		return columns.size();
	}

	/**
	 * @return whether or not the size taken up by binary stored records of this schema varies at run-time (i.e. depending on data input)
	 */
	public boolean isVariableSize()
	{
		return isVariableSize(false, null);
	}
	
	/**
	 * @return whether or not the size taken up by binary stored records of this schema varies at run-time (i.e. depending on data input)
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore in the total
	 * @return
	 */
	public boolean isVariableSize(boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		for(Column<?> c : getColumns(includeVirtual))
			if((skipColumns == null || !skipColumns.contains(c)) && c.isVariableSize())
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
		return getMinimumSize(false, null);
	}
	
	/**
	 * Returns the minimum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore in the total
	 * @return
	 */
	public int getMinimumSize(boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		int total = 0;
		for(Column<?> c : getColumns(includeVirtual))
			if(skipColumns == null || !skipColumns.contains(c))
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
		return getMaximumSize(false, null);
	}

	/**
	 * Returns the maximum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore the total
	 * @return
	 */
	public int getMaximumSize(boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		int total = 0;
		for(Column<?> c : getColumns(includeVirtual))
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
		if(this == obj) // compare pointers first
			return true;
		if(obj instanceof Schema)
		{
			Schema other = (Schema) obj;
			// ID:
			boolean idMatch = (this.id == other.id);
			if(!(checkNames || checkColumns) || !idMatch)
				return idMatch;
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
					if(!myCols.next().equals(otherCols.next(), checkNames, true))
						return false;
				//TODO compare indexes
				//TODO compare virtualColumns
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
		hash = 31 * hash + (int)(id ^ (id >>> 32));
		hash = 31 * hash + (name == null ? 0 : name.hashCode());
		hash = 31 * hash + columns.hashCode();
		hash = 31 * hash + columnToVirtualColumns.hashCode();
		hash = 31 * hash + indexes.hashCode();
		hash = 31 * hash + (primaryKey == null ? 0 : primaryKey.hashCode());
		hash = 31 * hash + (sealed ? 0 : 1);
		return hash;
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
		for(Column<?> c : getColumns(true))
			bff.append("\n\t- " + c.getSpecification());
		return bff.toString();
	}
	
	public void accept(ColumnVisitor visitor)
	{
		accept(visitor, null);
	}
	
	public void accept(ColumnVisitor visitor, Set<Column<?>> skipColumns)
	{
		for(Column<?> c : getColumns(visitor.includeVirtualColumns()))
			if(skipColumns == null || !skipColumns.contains(c))
				c.accept(visitor);
	}

}
