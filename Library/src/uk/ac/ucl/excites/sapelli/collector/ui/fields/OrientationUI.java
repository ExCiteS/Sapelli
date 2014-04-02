/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI;

/**
 * @author mstevens
 *
 */
public abstract class OrientationUI<V> extends SelfLeavingFieldUI<OrientationField, V>
{

	public OrientationUI(OrientationField field, Controller controller, CollectorUI<V> collectorUI)
	{
		super(field, controller, collectorUI);
	}

}
