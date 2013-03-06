/**
 * 
 */
package uk.ac.ucl.excites.collector;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.EndField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.FormEntry;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.ButtonsState;
import uk.ac.ucl.excites.collector.util.DeviceID;
import uk.ac.ucl.excites.collector.util.LocationUtils;
import android.content.Context;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.os.Bundle;
import android.os.Vibrator;

/**
 * @author mstevens, Michalis Vitos
 * 
 */
public class ProjectController implements LocationListener
{

	@SuppressWarnings("unused")
	static private final String TAG = "ProjectController";

	private Project project;
	private DataAccess dao;
	private CollectorActivity activity;

	private long deviceID; // 32bit unsigned CRC32 hashcode

	private Form currentForm;
	private FormEntry entry;
	private Field currentField;
	private Stack<Field> fieldHistory;

	private LocationManager locationManager;
	private Location currentBestLocation = null;

	public ProjectController(Project project, DataAccess dao, CollectorActivity activity)
	{
		this.project = project;
		this.dao = dao;
		this.activity = activity;

		fieldHistory = new Stack<Field>();
		deviceID = (new DeviceID(activity)).getCRC32Hash();
	}

	public void startProject()
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
		// Clear stuff:
		fieldHistory.clear();
		currentField = null;

		// Create new record:
		if(!dao.isOpen())
			dao.openDB();
		entry = currentForm.newEntry(dao, deviceID);

		// Location...
		List<LocationField> lfStartWithForm = currentForm.getLocationFields(true);
		if(!lfStartWithForm.isEmpty())
			startLocationListener(lfStartWithForm); // start listening for location updates
		else
			stopLocationListener(); // stop listening for location updates

		// Begin completing the form at the start field:
		goTo(currentForm.getStart());
	}

	public void goForward()
	{
		if(currentField != null)
			goTo(currentForm.getNextField(currentField));
		else
			restartForm(); // this shouldn't happen
	}

	public void goBack()
	{
		if(!fieldHistory.isEmpty())
		{
			currentField = null; // !!! otherwise we create loops
			goTo(fieldHistory.pop());
		}
	}

	public void goTo(Field nextField)
	{
		// Leafing current field...
		if(currentField != null)
			fieldHistory.add(currentField); // Add to history
		// Entering next field...
		currentField = nextField;
		// Handle LocationField:
		if(currentField instanceof LocationField)
		{
			LocationField lf = (LocationField) currentField;
			if(lf.isWaitAtField() || lf.storeLocation(LocationUtils.getExCiteSLocation(currentBestLocation), entry))
				startLocationListener(lf); // start listening for a location
			else
			{ // we already have a location
				goForward(); // skip the wait screen
				return; // !!!
			}
		}
		// Update GUI or loop/exit
		if(!(currentField instanceof EndField))
			activity.setField(currentField, !fieldHistory.empty(), !fieldHistory.empty(), false); // update GUI
		else
			endForm(); // currentField = _END, so we must loop or exit
	}

	public ButtonsState getButtonsState()
	{
		ButtonsState state = new ButtonsState(currentForm.isShowBack() && !fieldHistory.empty(), currentForm.isShowCancel() && !fieldHistory.empty(),
				currentForm.isShowForward() && false /* for now we don't use the forward button */);
		// Note: these paths may be null (in which case built-in defaults must be used)
		state.setBackImagePath(currentForm.getBackImagePath());
		state.setCancelImagePath(currentForm.getCancelImagePath());
		state.setForwardImagePath(currentForm.getForwardImagePath());
		return state;
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
		// TODO
		// Store/increase number of photos taken
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
		//Store entry
		entry.store(); // saves entry in database

		//Signal the successful storage of the entry
		// Vibration
		if(currentForm.isVibrateOnEnd())
		{
			Vibrator vibrator = (Vibrator) activity.getSystemService(Context.VIBRATOR_SERVICE);
			// Vibrate for 600 milliseconds
			vibrator.vibrate(600);
		}
		// Play sound
		String endSound = currentForm.getEndSoundPath();
		if(endSound != null && !endSound.isEmpty())
		{
			File endSoundPath = new File(project.getSoundPath() + endSound);	
			if(endSoundPath.exists()) //check if the file really exists
			{
				// Play the sound
				MediaPlayer mp = MediaPlayer.create(activity, Uri.fromFile(endSoundPath));
				mp.start();
				mp.setOnCompletionListener(new OnCompletionListener()
				{
					@Override
					public void onCompletion(MediaPlayer mp)
					{
						mp.release();
					}
				});
			}
		}

		//End action:
		switch(currentForm.getEndAction())
		{
			case Form.END_ACTION_LOOP:
				restartForm();
				break;
			case Form.END_ACTION_EXIT:
				activity.finish();
				break; // leaves the application!
		}
	}

	/**
	 * @return the currentForm
	 */
	public Form getCurrentForm()
	{
		return currentForm;
	}

	/**
	 * @return the project
	 */
	public Project getProject()
	{
		return project;
	}

	private void startLocationListener(LocationField locField)
	{
		startLocationListener(Arrays.asList(locField));
	}

	private void startLocationListener(List<LocationField> locFields)
	{
		if(locFields.isEmpty())
			return;
		// get locationmanager:
		if(locationManager == null)
			locationManager = (LocationManager) activity.getSystemService(Context.LOCATION_SERVICE);
		// deteriment which provider(s) we need:
		Set<String> providers = new HashSet<String>();
		for(LocationField lf : locFields)
			providers.addAll(LocationUtils.getProvider(locationManager, lf));
		// start listening to each provider:
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
		if(LocationUtils.isBetterLocation(location, currentBestLocation))
			currentBestLocation = location;

		// boolean keepListening = false;
		// for(LocationField lf : currentForm.getLocationFields())
		// {
		// boolean stored = lf.storeLocation(LocationUtils.getExCiteSLocation(location), entry);
		//
		// keepListening |= (lf.isWaitAtField() && currentField != lf);
		// }
		// if(!keepListening)
		// stopLocationListener();
		//

		// avoid overwrite after field?

		// if(currentField instanceof LocationField)
		// { //user is waiting for a location for the currentfield
		// activity.stopLocationTimer(); //stop waiting screen timer!
		// stopLocationListener(); //stop listening for locations
		// ((LocationField) currentField).storeLocation(LocationUtils.getExCiteSLocation(location), entry); //store location
		//
		// goForward(); //continue (will leave waiting screen)
		// }
		// else if(currentForm.getLocationFields().size() == 1)
		// {
		// LocationField lf = currentForm.getLocationFields().get(0);
		// lf.storeLocation(LocationUtils.getExCiteSLocation(location), entry); //store location
		// if(!lf.isWaitAtField())
		// stopLocationListener();
		// }
		// else
		// { //this should not happen really...
		//
		// }
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
