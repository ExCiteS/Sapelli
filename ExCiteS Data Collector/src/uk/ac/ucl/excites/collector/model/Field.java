package uk.ac.ucl.excites.collector.model;

import java.util.List;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public abstract class Field
{

	protected String id;
	protected Field jump;
	protected boolean noColumn;
	
	/**
	 * @return the id
	 */
	public String getID()
	{
		return id;
	}

	/**
	 * @param id the id to set
	 */
	public void setID(String id)
	{
		this.id = id;
	}

	/**
	 * @return the noColumn
	 */
	public boolean isNoColumn()
	{
		return noColumn;
	}

	/**
	 * @param noColumn the noColumn to set
	 */
	public void setNoColumn(boolean noColumn)
	{
		this.noColumn = noColumn;
	}

	public void setJump(Field target)
	{
		this.jump = target;
	}
	
	public Field getJump()
	{
		return jump;
	}
	
	public void addColumns(Schema schema)
	{
		if(!noColumn)
			_addColumns(schema);
	}
	
	protected abstract void _addColumns(Schema schema);
	
}
