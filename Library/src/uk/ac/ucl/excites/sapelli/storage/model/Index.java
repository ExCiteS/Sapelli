/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.util.List;
import java.util.Arrays;

/**
 * Class representing a database index spanning one or more columns
 * 
 * @author mstevens
 */
public class Index
{

	private String name;
	private List<Column<?>> columns;
	private boolean unique;
	
	public Index(String name, boolean unique, Column<?>... columns)
	{
		this.name = name;
		this.unique = unique;
		this.columns = Arrays.asList(columns);
	}

	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * @return the columns
	 */
	public List<Column<?>> getColumns()
	{
		return columns;
	}

	/**
	 * @return the unique
	 */
	public boolean isUnique()
	{
		return unique;
	}
	
}
