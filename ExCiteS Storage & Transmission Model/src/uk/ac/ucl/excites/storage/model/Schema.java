package uk.ac.ucl.excites.storage.model;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;

/**
 * @author mstevens
 *
 */
@SuppressWarnings("rawtypes")
public class Schema
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
	//TODO Do more Googling to find if there is a HashMap data structure that can return the keys as an array or ArrayList (in insertion order)?
	//		In that case we don't need to have this separate arraylist
	
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
		return columns.get(index);
	}
	
	public Column getColumn(String name)
	{
		return columns.get(columnNameToIndex.get(name));
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
	 * Returns the number of bits records of this schema take up when written to a binary representation.
	 * In case of a variable size the maximum effective size is returned.
	 * 
	 * @return
	 */
	public int getSize()
	{
		int total = 0;
		for(Column<?> c : columns)
			total += c.getSize();
		return total;
	}
	
}
