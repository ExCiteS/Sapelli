/**
 * 
 */
package uk.ac.ucl.excites.collector;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.EndField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.FormEntry;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.util.LocationUtils;
import android.content.Context;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;

/**
 * @author mstevens
 * 
 */
public class ProjectController implements LocationListener
{

	private Project project;
	private DataAccess dao;
	private Form currentForm;
	private FormEntry entry;
	private Field currentField;
	private CollectorActivity activity;
	private LocationManager locationManager;

	private long deviceID;

	private Stack<Field> fieldHistory;

	public ProjectController(Project project, DataAccess dao, CollectorActivity activity)
	{
		this.project = project;
		this.dao = dao;
		this.activity = activity;

		fieldHistory = new Stack<Field>();

		// TODO get device ID
		deviceID = 0;
	}
	
	public void startProject ()
	{
		startForm(0); // For now projects have only one form
	}

	public void startForm(String formName)
	{
		// Find form with the given name:
		Form form = null;
		for(Form f : project.getForms())
			if(f.getName().equals(formName))
			{
				form = f;
				break;
			}
		if(form != null)
			startForm(form);
		else
			throw new IllegalArgumentException("Form " + formName + " could not be found in this project.");
	}

	public void startForm(int formIndex)
	{
		startForm(project.getForms().get(formIndex));
	}

	public void startForm(Form form)
	{
		currentForm = form;
		restartForm();
	}

	public void restartForm()
	{
		fieldHistory.clear();

		entry = currentForm.newEntry(dao); // pass deviceID

		currentField = null;
		goTo(currentForm.getStart());

		if(currentForm.hasLocationField())
		{ // start listening for location updates:
			if(locationManager == null)
				locationManager = (LocationManager) activity.getSystemService(Context.LOCATION_SERVICE);
			List<String> providers = new ArrayList<String>();
			switch(currentForm.getLocationField().getType())
			{
			case LocationField.TYPE_GPS:
				providers.add(LocationManager.GPS_PROVIDER);
				break;
			case LocationField.TYPE_NETWORK:
				providers.add(LocationManager.NETWORK_PROVIDER);
				break;
			// others later?
			case LocationField.TYPE_ANY:
				providers = locationManager.getAllProviders();
				break;
			default:
				providers.add(LocationManager.GPS_PROVIDER);
			}
			for(String p : providers)
				locationManager.requestLocationUpdates(p, LocationField.LISTENER_UPDATE_MIN_TIME_MS, LocationField.LISTENER_UPDATE_MIN_DISTANCE_M, this);
		}
		else
		{ // stop listening for updates:
			if(locationManager != null)
				locationManager.removeUpdates(this);
		}
	}

	public void goForward()
	{
		if(currentField != null)
			goTo(currentForm.getNextField(currentField));
		else
			restartForm(); // this shouldn't happen
	}

	public void goBack() // TODO fix bug (-> runs in a loop) + don't show Button when go back to first screen
	{
		if(!fieldHistory.isEmpty())
			goTo(fieldHistory.pop());
	}

	public void goTo(Field nextField)
	{
		// Leafing current field...
		if(currentField != null)
			fieldHistory.add(currentField); // Add to history
		// Entering next field...
		currentField = nextField;
		// Location
		if(currentField == currentForm.getLocationField())
		{
			if(currentForm.getLocationField().retrieveLocation(entry) != null)
			{ // we have a location
				goForward(); // skip the wait screen
				return; // !!!
			}
		}
		// Update GUI or loop/exit
		if(currentField != EndField.getInstance())
			activity.setField(currentField, !fieldHistory.empty(), !fieldHistory.empty(), false); // update GUI
		else
			endForm(); // currentField = _END, so we must loop or exit
	}

	/**
	 * To be called from UI/CollectorActivity
	 * 
	 * @param chosenChild
	 */
	public void choiceMade(Choice chosenChild)
	{
		// Note: chosenChild is not the currentField! The currentField (also a Choice) is its parent.
		if(chosenChild.isLeaf())
		{
			// Store value
			if(!chosenChild.getRoot().isNoColumn())
				chosenChild.storeValue(entry);
			// Go to next field
			goTo(currentForm.getNextField(chosenChild));
			/*
			 * We cannot use goForward() here because then we would first need to make the chosenChild the currentField, in which case it would end up in the
			 * fieldHistory which does not make sense because a leaf choice cannot be displayed on its own.
			 */
		}
		else
			goTo(chosenChild); // chosenChild becomes the new currentField (we go one level down in the choice tree)
	}

	@Override
	public void onLocationChanged(Location location)
	{
		// Store the location:
		currentForm.getLocationField().storeLocation(LocationUtils.getExCiteSLocation(location), entry);
		// Check if waiting screen is shown:
		if(currentField instanceof LocationField)
		{ // this means the "waiting for location" screen is currently showed
			activity.stopLocationTimer(); // stop timer!
			goForward(); // stop waiting
		}
	}

	public void photoDone(boolean pictureTaken)
	{
		// Store/increase number of photos taken
		// TODO
		// goto next/jump field:
		goForward();
	}

	public void audioDone(boolean recordingMade)
	{
		// Store/increase number of recordings taken
		// TODO
		// goto next/jump field:
		goForward();
	}

	public void endForm()
	{
		// Store entry
		entry.store(); // saves entry in database
		// Play sound/ vibrate
		// TODO
		// End action:
		switch(currentForm.getEndAction())
		{
		case Form.END_ACTION_LOOP:
			restartForm();
			break;
		case Form.END_ACTION_EXIT:
			activity.finish();
			break; // leaves the application!
		// TODO default :
		}
	}

	@Override
	public void onProviderDisabled(String provider)
	{
		// does nothing for now
	}

	@Override
	public void onProviderEnabled(String provider)
	{
		// does nothing for now
	}

	@Override
	public void onStatusChanged(String provider, int status, Bundle extras)
	{
		// does nothing for now
	}

}
