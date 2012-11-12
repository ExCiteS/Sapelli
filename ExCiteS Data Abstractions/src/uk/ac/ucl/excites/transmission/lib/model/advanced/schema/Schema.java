package uk.ac.ucl.excites.transmission.lib.model.advanced.schema;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author mstevens
 *
 */
public class Schema
{

	private int id;
	private String name;
	private String description;
	private List<Column> columns;
	
	public Schema(int id)
	{
		this.id = id;
		columns = new ArrayList<Column>();
	}
	
	public Schema(int id, String name)
	{
		this(id);
		this.name = name;
	}
	
	public void addColumn(Column col)
	{
		columns.add(col);
	}
	
	public List<Column> getColumns()
	{
		return columns;
	}

}
