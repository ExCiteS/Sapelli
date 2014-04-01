/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;

/**
 * @author mstevens
 *
 */
public abstract class OrientationUI<V> extends FieldUI<OrientationField, V>
{

	public OrientationUI(OrientationField field, Controller controller, CollectorUI<V> collectorUI)
	{
		super(field, controller, collectorUI);
	}

	@Override
	public boolean leave(CollectorRecord record, boolean noValidation)
	{
		return noValidation || isValid(record);
	}

	@Override
	public boolean isValid(CollectorRecord record)
	{
		return field.getColumn().isValueSet(record) || field.getOptional() == Optionalness.ALWAYS;
	}

}
