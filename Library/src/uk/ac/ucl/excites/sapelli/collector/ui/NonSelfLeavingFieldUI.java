/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;

/**
 * Superclass for FieldUIs that can only be left due to external request (e.g. the forward button being pressed) and
 * therefore need to execute validation and data storage behaviour at that moment.
 * 
 * @param <F>
 * @param <V>
 * 
 * @author mstevens
 */
public abstract class NonSelfLeavingFieldUI<F extends Field, V> extends FieldUI<F, V>
{

	public NonSelfLeavingFieldUI(F field, Controller controller, CollectorUI<V> collectorUI)
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
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#leave(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord, boolean)
	 */
	@Override
	public boolean leave(CollectorRecord record, boolean noValidation)
	{
		if(noValidation || isValid(record))
		{
			if(!field.isNoColumn())
				storeValue(record);
			return true;
		}
		return false;
	}
	
	/**
	 * It can safely assumed that the field#noColumn=false (except in the case of Pages) and that isValid() has been called beforehand and returned true.
	 * 
	 * @param record
	 */
	protected abstract void storeValue(CollectorRecord record);

}
