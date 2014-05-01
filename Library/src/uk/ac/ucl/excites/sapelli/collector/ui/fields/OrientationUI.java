/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;

/**
 * @author mstevens
 *
 */
public abstract class OrientationUI<V, UI extends CollectorUI<V, UI>> extends SelfLeavingFieldUI<OrientationField, V, UI>
{

	public OrientationUI(OrientationField field, Controller controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}

}
