/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
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
	 * Leaving is only allowed upon successful validation, and if valid the value must first be stored.
	 * 
	 * Note: "field group" fields (currently there is only Page, possibly we'll introduce others later) will need to
	 * override this method in order to avoid the noColumn check on themselves and to call leave() on each of their
	 * contained fields. 
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#leave(Record, boolean)
	 */
	@Override
	public boolean leave(Record record, boolean noValidation)
	{
		if(noValidation || isValid(record))
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
					return false;
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

}
