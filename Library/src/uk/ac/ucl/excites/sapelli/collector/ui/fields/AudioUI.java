/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;

/**
 * @author mstevens
 * 
 */
public abstract class AudioUI<V, UI extends CollectorUI<V, UI>> extends MediaUI<AudioField, V, UI>
{
	
	public AudioUI(AudioField field, Controller controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}

}
