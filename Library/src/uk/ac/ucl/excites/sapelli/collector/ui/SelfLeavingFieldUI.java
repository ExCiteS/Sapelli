/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;

/**
 * Super class for FieldUIs that "leave themselves", rather than requiring an external request for that.
 * It is assumed they will handle and validation or storage behaviour themselves before the calling of
 * leave(Record), so no validation or storage is repeated at that point.
 * 
 * Note 1: subclasses still need to implements isValid() such that we can check if they are in a valid
 * state from the outside (e.g. to either show or hide the forward button on non-optional but revisited
 * fields that have already acquired a value).
 * 
 * Note 2: if the field has optionality = ALWAYS it will still be possible to leave it by means of external
 * request (i.e. the forward button being pressed), when this happens it is implied that the value no value
 * has to be stored, nor validated, for that matter.
 * 
 * @param <F>
 * @param <V>
 * 
 * @author mstevens
 */
public abstract class SelfLeavingFieldUI<F extends Field, V> extends FieldUI<F, V>
{

	public SelfLeavingFieldUI(F field, Controller controller, CollectorUI<V> collectorUI)
	{
		super(field, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#leave(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord, boolean)
	 */
	@Override
	public boolean leave(CollectorRecord record, boolean noValidation)
	{
		return true; // no validation or storage logic at leaving time.
	}

}
