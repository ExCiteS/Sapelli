package uk.ac.ucl.excites.storage.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;

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
	
	private LinkedHashMap<String,Column> columnsMap; 
	private ArrayList<Column> columnsArray;
	//TODO Do more google to find if there is a HashMap data structure that can return the keys as an array or ArrayList (in insertion order)?
	//		In that case we don't need there separate arraylist
	
	private boolean sealed = false;
	
	private Table<Record, Column, Object> table;
	
	//TODO epicollect URL?
	//TODO excites URL?
	
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
		columnsMap = new LinkedHashMap<String, Column>();
		columnsArray = new ArrayList<Column>();
	}
	
	protected void addColumn(Column column)
	{
		if(!sealed)
		{
			columnsMap.put(column.getName(), column);
			columnsArray.add(column);
			column.setSchema(this);
		}
		else
			throw new IllegalStateException("Cannot extend a sealed schema!");
	}
	
	public Column getColumn(int index)
	{
		return columnsArray.get(index);
	}
	
	public Column getColumn(String name)
	{
		return columnsMap.get(name);
	}
	
	public Collection<Column> getColumns()
	{
		return columnsArray;
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
		this.table = HashBasedTable.create(); //TODO try different Table implementations (see Guava docs)
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
	 * @return the records
	 */
	public Table<Record, Column, Object> getTable()
	{
		return table;
	}

}
