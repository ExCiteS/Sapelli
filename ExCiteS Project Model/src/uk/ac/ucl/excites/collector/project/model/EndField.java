/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.storage.model.Column;

/**
 * Dummy field to represent the end of any form.
 * Implements the Singleton design pattern, so there can only be one instance of this class.
 * 
 * @author mstevens
 */
public class EndField extends Field
{

	static public final String ID = "_END";
	
	static private EndField INSTANCE;
	
	static public EndField getInstance()
	{
		if(INSTANCE == null)
			INSTANCE = new EndField();
		return INSTANCE;
	}
	
	private EndField()
	{
		super(ID);
	}

	@Override
	protected Column<?> createColumn()
	{
		return null;
	}

}
