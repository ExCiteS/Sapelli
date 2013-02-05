package uk.ac.ucl.excites.collector.model;

import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public abstract class Field
{

	protected String id;
	protected Field jump;
	protected boolean enabled = true;
	protected boolean noColumn;
	
	public Field(String id)
	{
		this.id = id;
	}
	
	/**
	 * @return the id
	 */
	public String getID()
	{
		return id;
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
	
	/**
	 * @return the enabled
	 */
	public boolean isEnabled()
	{
		return enabled;
	}
	
	public void disable()
	{
		enabled = false;
	}
	
	public void enable()
	{
		enabled = true;
	}
	
	public abstract void addColumns(Schema schema);
	
	public abstract void storeValues(Record record);
	
}
