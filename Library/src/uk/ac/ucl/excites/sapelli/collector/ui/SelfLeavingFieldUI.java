/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;

/**
 * Super class for FieldUIs that can "leave themselves", rather than requiring an external request for that.
 * It is assumed they will handle any validation and storage behaviour themselves _before_ the calling of leave().
 * However, there are cases in which it will still be possible to leave it by means of external request
 * (one obvious example is the pressing of the forward button on a field with optionality=ALWAYS).
 * Therefore, basic validation of stored values is repeated at the point of leaving, but storage itself is not!
 * 
 * Note: subclasses may override isValid() to perform more specific checks (or no check at all if that is appropriate).
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
		return noValidation || isValid(record); // no storage happens at this point!
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(CollectorRecord record)
	{
		return field.isNoColumn() || field.getOptional() == Optionalness.ALWAYS || field.getColumn().isValueSet(record);
	}

}
