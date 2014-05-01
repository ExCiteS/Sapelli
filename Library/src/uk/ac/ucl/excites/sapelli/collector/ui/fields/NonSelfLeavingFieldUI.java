/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Superclass for FieldUIs that can only be left due to external request (e.g. the forward button being pressed) and
 * therefore need to execute validation and data storage behaviour at that moment.
 * 
 * @param <F>
 * @param <V>
 * @param <UI>
 * 
 * @author mstevens
 */
public abstract class NonSelfLeavingFieldUI<F extends Field, V, UI extends CollectorUI<V, UI>> extends FieldUI<F, V, UI>
{

	public NonSelfLeavingFieldUI(F field, Controller controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}

	/**
	 * Unless noValidation=true, leaving is only allowed upon successful validation, and if valid the value must first be stored.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#leave(Record, boolean)
	 */
	@Override
	protected boolean leave(Record record, boolean skipValidation)
	{
		if(skipValidation || isValid(record))
		{
			if(!field.isNoColumn())
			{
				try
				{
					storeValue(record);
				}
				catch(Exception e)
				{
					e.printStackTrace(System.err);
					controller.addLogLine("STORAGE ERROR", e.getClass().getName(), (e.getMessage() != null ? e.getMessage() : ""));
					return skipValidation;
				}
			}
			return true;
		}
		return false;
	}
	
	/**
	 * It can safely assumed that the field#noColumn=false (except in the case of Pages) and that isValid() has been called beforehand and returned true.
	 * 
	 * @param record
	 * @throws Exception 
	 */
	protected abstract void storeValue(Record record) throws Exception;

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isShowForward()
	 */
	@Override
	protected boolean isShowForward()
	{
		return true;
	}
	
}
