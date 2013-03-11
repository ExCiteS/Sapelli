package uk.ac.ucl.excites.storage.model;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author mstevens
 *
 */
@SuppressWarnings("rawtypes")
public class Schema
{

	public static final int DEFAULT_VERSION = 0;
	
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
		this.id = id;
		this.version = version;
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
		return columnNameToIndex.get(name);
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

}
