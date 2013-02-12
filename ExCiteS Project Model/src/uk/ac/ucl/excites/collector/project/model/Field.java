package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public abstract class Field
{
	
	//Statics----------------------------------------------
	
	//Defaults:
	static public final boolean DEFAULT_ENABLED = true;
	static public final boolean DEFAULT_OPTIONAL = true;
	static public final boolean DEFAULT_NO_COLUMN = false;
	
	//Dynamics---------------------------------------------
	protected String id;
	protected Field jump;
	protected boolean enabled = DEFAULT_ENABLED;
	protected boolean optional = DEFAULT_OPTIONAL;
	protected boolean noColumn = DEFAULT_NO_COLUMN;
	
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

	/**
	 * @return the optional
	 */
	public boolean isOptional()
	{
		return optional;
	}

	/**
	 * @param optional the optional to set
	 */
	public void setOptional(boolean optional)
	{
		this.optional = optional;
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
	
}
