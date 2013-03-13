/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
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
	
	static public final String ID(Form form)
	{
		return ID + "_" + form.getName();
	}
	
	public EndField(Form form)
	{
		super(form, ID(form));
		noColumn = true;
	}

	@Override
	protected Column<?> createColumn()
	{
		return null;
	}

	@Override
	public void setIn(CollectorUI fv)
	{
		//DOES NOTHING! (This should never be called)
	}

}
