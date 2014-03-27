/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.Form.Next;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;

/**
 * @author mstevens
 *
 */
public abstract class LocationUI<V> extends FieldUI<LocationField, V>
{

	public LocationUI(LocationField field, Controller controller, CollectorUI<V> collectorUI)
	{
		super(field, controller, collectorUI);
	}

	@Override
	public boolean leave(CollectorRecord record, boolean noValidation)
	{
		// TODO Auto-generated method stub
		return fals;
	}

	@Override
	public boolean isValid(CollectorRecord record)
	{
		// TODO Auto-generated method stub
		return fals;
	}
	
	@Override
	public abstract void cancel();
	
	protected void timeout()
	{
		if(field != controller.getCurrentField())
			return; // this shouldn't happen really
		
		//Log:
		controller.addLogLine("TIMEOUT", field.getID());
		
		CollectorRecord record = controller.getCurrentRecord();
		
		if(field.retrieveLocation(record) == null && field.isUseBestNonQualifyingLocationAfterTimeout())
			field.storeLocation(record, controller.getCurrentBestLocation(), true);
			
		// If still no location set (because either isUseBestNQLAT==false or currentBestLocation==null), and locationField is non-optional: cancel & exit!
		if(field.retrieveLocation(record) == null && field.getOptional() != Optionalness.ALWAYS)
		{
			activity.showErrorDialog("Cannot get GPS signal and location is mandatory for field '" + field.getID() + "'. Please, make sure your GPS receiver is enabled.", true, new Runnable()
			{
				@Override
				public void run()
				{
					goTo(new EndField(currFormSession.form, false, Next.EXITAPP)); // called controller.exit ... instead?
				}
			});
			return;
		}
		
		controller.goForward(false);
	}

}
