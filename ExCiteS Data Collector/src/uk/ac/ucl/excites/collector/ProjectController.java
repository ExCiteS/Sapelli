/**
 * 
 */
package uk.ac.ucl.excites.collector;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import android.content.Context;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;

import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.EndField;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.FormEntry;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.util.DeviceID;
import uk.ac.ucl.excites.collector.util.LocationUtils;

import uk.ac.ucl.excites.collector.util.LocationUtils;
import android.content.Context;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;

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
	
	private long deviceID; //32bit unsigned CRC32 hashcode
	
	private Stack<Field> fieldHistory;

	public ProjectController(Project project, DataAccess dao, CollectorActivity activity)
	{
		this.project = project;
		this.dao = dao;
		this.activity = activity;

		fieldHistory = new Stack<Field>();
		
		deviceID = (new DeviceID(activity)).getCRC32Hash();
	}
	
	public void startProject (){
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
		//Clear stuff:
		fieldHistory.clear();
		currentField = null;
		
		//Create new record:
		entry = currentForm.newEntry(dao, deviceID); //TODO pass deviceID

		//Handle locationfield(s):
		
		
		
		if(currentForm.getLocationFields().size() > 0)
		{

		
		}
		else
		{
			stopLocationListener();
		}
		
		//Begin completing the form at the start field:
		goTo(currentForm.getStart());
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
		//Handle LocationField:
		if(currentField instanceof LocationField)
		{
			LocationField lf = (LocationField) currentField;
			if(lf.retrieveLocation(entry) == null)
				startLocationListener(lf); //start listening for a location
			else
			{	//we already have a location
				goForward(); //skip the wait screen
				return; //!!!
			}
		}
		// Update GUI or loop/exit
		if(currentField != EndField.getInstance())
			activity.setField(currentField, !fieldHistory.empty(), !fieldHistory.empty(), false); // update GUI
		else
			endForm(); // currentField = _END, so we must loop or exit
	}

	/**
	 * To be called from ChoiceView
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

	private void startLocationListener(LocationField locField)
	{
		//TODO
	}
	
	private void startLocationListener(List<LocationField> locFields)
	{
		//start listening for location updates:
		if(locationManager == null)
			locationManager = (LocationManager) activity.getSystemService(Context.LOCATION_SERVICE);
		Set<String> providers = new HashSet<String>();
			for(LocationField lf : locFields)
				providers.addAll(LocationUtils.getProvider(locationManager, lf));
		for(String p : providers)
			locationManager.requestLocationUpdates(p, LocationField.LISTENER_UPDATE_MIN_TIME_MS, LocationField.LISTENER_UPDATE_MIN_DISTANCE_M, this);
	}
	
	private void stopLocationListener()
	{
		if(locationManager != null)
			locationManager.removeUpdates(this);
	}
	
	@Override
	public void onLocationChanged(Location location)
	{
		boolean keepListening = false;
		for(LocationField lf : currentForm.getLocationFields())
		{
			boolean stored = lf.storeLocation(LocationUtils.getExCiteSLocation(location), entry);
			
			keepListening |= (lf.isWaitAtField() && currentField != lf);
		}
		if(!keepListening)
			stopLocationListener();
		
		
		//avoid overwrite after field?
		
		
//		if(currentField instanceof LocationField)
//		{	//user is waiting for a location for the currentfield
//			activity.stopLocationTimer(); //stop waiting screen timer!
//			stopLocationListener(); //stop listening for locations
//			((LocationField) currentField).storeLocation(LocationUtils.getExCiteSLocation(location), entry); //store location 
//			
//			goForward(); //continue (will leave waiting screen)
//		}
//		else if(currentForm.getLocationFields().size() == 1)
//		{
//			LocationField lf = currentForm.getLocationFields().get(0);
//			lf.storeLocation(LocationUtils.getExCiteSLocation(location), entry); //store location 
//			if(!lf.isWaitAtField())
//				stopLocationListener();
//		}
//		else
//		{	//this should not happen really...
//			
//		}
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
