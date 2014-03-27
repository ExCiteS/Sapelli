/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
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
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isValid(CollectorRecord record)
	{
		// TODO Auto-generated method stub
		return false;
	}

}
