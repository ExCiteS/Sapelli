package uk.ac.ucl.excites.storage.model;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;

/**
 * @author mstevens
 * 
 */
@SuppressWarnings("rawtypes")
public class Schema implements Comparable<Schema>
{

	public static final int DEFAULT_VERSION = 0;
	public static final int UNKNOWN_COLUMN_INDEX = -1;

	public static final int SCHEMA_ID_SIZE = 24;
	public static final IntegerRangeMapping SCHEMA_ID_FIELD = IntegerRangeMapping.ForSize(0, SCHEMA_ID_SIZE);
	public static final int SCHEMA_VERSION_SIZE = 8;
	public static final IntegerRangeMapping SCHEMA_VERSION_FIELD = IntegerRangeMapping.ForSize(0, SCHEMA_VERSION_SIZE);

	private int id;
	private int version;
	private String name;
	private String description;

	private ArrayList<Column> columns;
	private Map<String, Integer> columnNameToIndex;
	// TODO Do more Googling to find if there is a HashMap data structure that can return the keys as an array or ArrayList (in insertion order)?
	// In that case we don't need to have this separate arraylist

	private boolean sealed = false;

	public Schema(int id)
	{
		this(id, DEFAULT_VERSION, null);
	}

	public Schema(int id, String name)
	{
		this(id, DEFAULT_VERSION, name);
	}

	public Schema(int id, int version)
	{
		this(id, version, null);
	}

	public Schema(int id, int version, String name)
	{
		if(SCHEMA_ID_FIELD.fits(id))
			this.id = id;
		else
			throw new IllegalArgumentException("Invalid schema ID, valid values are " + SCHEMA_ID_FIELD.getLogicalRangeString() + ".");
		if(SCHEMA_VERSION_FIELD.fits(version))
			this.version = version;
		else
			throw new IllegalArgumentException("Invalid schema version, valid values are " + SCHEMA_VERSION_FIELD.getLogicalRangeString() + ".");
		this.name = (name == null ? "Schema_" + id + "_v" + version : name);
		columnNameToIndex = new LinkedHashMap<String, Integer>();
		columns = new ArrayList<Column>();
	}

	public void addColumn(Column column)
	{
		if(!sealed)
		{
			columnNameToIndex.put(column.getName(), columns.size());
			columns.add(column);
			column.setSchema(this);
		}
		else
			throw new IllegalStateException("Cannot extend a sealed schema!");
	}

	public Column getColumn(int index)
	{
		try
		{
			return columns.get(index);
		}
		catch(IndexOutOfBoundsException iobe)
		{
			return null;
		}
	}

	public Column getColumn(String name)
	{
		Integer idx = columnNameToIndex.get(name);
		if(idx == null)
			return null;
		return columns.get(idx);
	}

	public List<Column> getColumns()
	{
		return columns;
	}

	public int getColumnIndex(String name)
	{
		Integer idx = columnNameToIndex.get(name);
		if(idx == null)
			return UNKNOWN_COLUMN_INDEX;
		return idx.intValue();
	}

	public int getColumnIndex(Column column)
	{
		return columnNameToIndex.get(column.getName());
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
	 * @return the id
	 */
	public int getID()
	{
		return id;
	}

	/**
	 * @return the version
	 */
	public int getVersion()
	{
		return version;
	}

	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * @return the description
	 */
	public String getDescription()
	{
		return description;
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
	 * Compare method to sort schemata in order of ID and version (everything else is ignored)
	 * 
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Schema another)
	{
		int cmp = this.id - another.id;
		if(cmp == 0)
			cmp = this.version - another.version;
		return cmp;
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
		return equals(obj, false, false); // check only schema ID & version by default
	}

	/**
	 * Check if the provided object is an identical/equivalent Schema. Schema ID & version are always checked, names and columns are optionally checked, descriptions are ignored. 
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
			boolean idAndVersionMatch = (this.id == other.id) && (this.version == other.version);
			//ID & version:
			if(!(checkNames || checkColumns) || !idAndVersionMatch)
				return idAndVersionMatch;
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
		return "Schema [ID: " + id + "; version: " + version + "; name: " + name + "]";
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
