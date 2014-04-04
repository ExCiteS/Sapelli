/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;

/**
 * @author mstevens
 * 
 */
public abstract class PhotoUI<V, UI extends CollectorUI<V, UI>> extends MediaUI<PhotoField, V, UI>
{
	
	public PhotoUI(PhotoField field, Controller controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}

}
