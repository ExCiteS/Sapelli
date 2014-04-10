/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public abstract class LocationUI<V, UI extends CollectorUI<V, UI>> extends SelfLeavingFieldUI<LocationField, V, UI>
{

	public LocationUI(LocationField field, Controller controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}
	
	@Override
	public abstract void cancel(); // force concrete subclass to implement this (e.g. to stop listening for locations)!
	
	protected void timeout()
	{
		if(field != controller.getCurrentField())
			return; // this shouldn't happen really
		
		//Log:
		controller.addLogLine("TIMEOUT", field.getID());
		
		Record record = controller.getCurrentRecord();
		
		// Try to store current best non-qualifying location (if allowed):
		if(field.retrieveLocation(record) == null && field.isUseBestNonQualifyingLocationAfterTimeout())
			field.storeLocation(record, controller.getCurrentBestLocation(), true);
			
		// If still no location set (because either isUseBestNQLAT==false or currentBestLocation==null), and locationField is non-optional: loop form!
		if(isValid(record))
			controller.cancelAndRestartForm(); // TODO maybe show an error somehow?
		else
			controller.goForward(false);
	}

}
