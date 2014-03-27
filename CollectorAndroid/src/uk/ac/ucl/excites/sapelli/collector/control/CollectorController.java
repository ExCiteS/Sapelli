/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.control;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.activities.CollectorActivity;
import uk.ac.ucl.excites.sapelli.collector.geo.OrientationListener;
import uk.ac.ucl.excites.sapelli.collector.geo.OrientationSensor;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;
import uk.ac.ucl.excites.sapelli.collector.util.LocationUtils;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;
import uk.ac.ucl.excites.sapelli.util.DeviceControl;
import android.content.Context;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.util.Log;

/**
 * @author mstevens, Michalis Vitos, Julia
 * 
 */
public class CollectorController extends Controller implements LocationListener, OrientationListener
{

	// STATICS-------------------------------------------------------
	public static final String TAG = "CollectorController";
	public static final int LOCATION_LISTENER_UPDATE_MIN_TIME_MS = 15 * 1000;// 15 seconds
	public static final int LOCATION_LISTENER_UPDATE_MIN_DISTANCE_M = 5; // 5 meters

	// DYNAMICS------------------------------------------------------
	public CollectorActivity activity;

	private LocationManager locationManager;
	private Location currentBestLocation = null;
	private OrientationSensor orientationSensor;

	public CollectorController(Project project, CollectorView collectorView, RecordStore recordStore, CollectorActivity activity)
	{
		super(project, collectorView, recordStore);
		this.activity = activity;

		// Get Device ID (as a CRC32 hash):
		try
		{
			deviceIDHash = DeviceID.GetInstance(activity).getIDAsCRC32Hash();
		}
		catch(IllegalStateException ise)
		{
			activity.showErrorDialog("DeviceID has not be initialised!", true);
		}
	}

	@Override
	public boolean enterOrientationField(OrientationField of)
	{
		if(orientationSensor == null)
			orientationSensor = new OrientationSensor(activity);		
		orientationSensor.start(this); // start listening for orientation updates
		return false; // there is no UI needed for this (for now?) 
	}

	@Override
	public uk.ac.ucl.excites.sapelli.storage.types.Location getCurrentBestLocation()
	{
		return LocationUtils.getSapelliLocation(currentBestLocation); // passing null returns null
	}
	
	@Override
	protected void saveRecordAndAttachments()
	{	
		super.saveRecordAndAttachments(); //!!!
	
		// Also print the record on Android Log:
		if(currFormSession.form.isProducesRecords())
		{
			Log.d(TAG, "Stored record:");
			Log.d(TAG, currFormSession.record.toString());
		}
	}

	public void onOrientationChanged(Orientation orientation)
	{
		if(currFormSession.currField instanceof OrientationField)
		{
			((OrientationField) currFormSession.currField).storeValue(currFormSession.record, orientation);
			orientationSensor.stop(); // stop listening for updates
			goForward(false);
		}
	}

	public void startLocationListener(List<LocationField> locFields)
	{
		if(locFields.isEmpty())
			return;
		// get locationmanager:
		if(locationManager == null)
			locationManager = (LocationManager) activity.getSystemService(Context.LOCATION_SERVICE);
		// determine which provider(s) we need:
		Set<String> providers = new HashSet<String>();
		for(LocationField lf : locFields)
			providers.addAll(LocationUtils.getProvider(locationManager, lf));
		// start listening to each provider:
		for(String provider : providers)
			locationManager.requestLocationUpdates(provider, LOCATION_LISTENER_UPDATE_MIN_TIME_MS, LOCATION_LISTENER_UPDATE_MIN_DISTANCE_M, this);
	}

	public void stopLocationListener()
	{
		if(locationManager != null)
			locationManager.removeUpdates(this);
	}

	@Override
	public synchronized void onLocationChanged(Location location)
	{
		if(LocationUtils.isBetterLocation(location, currentBestLocation))
		{
			currentBestLocation = location;
			// check if we can/need to use the location now:
			if(currFormSession.currField instanceof LocationField)
			{	// user is currently waiting for a location for the currFormSession.currField
				LocationField lf = (LocationField) currFormSession.currField;
				// try to store location:
				if(lf.storeLocation(currFormSession.record, LocationUtils.getSapelliLocation(location)))
				{ // location successfully stored:
					if(currFormSession.form.getLocationFields(true).isEmpty())
						stopLocationListener(); // there are no locationfields with startWithForm=true (so there is no reason to keep listening for locations)
					goForward(false); // continue (will leave waiting screen & stop the timeout timer)
				}
				// else (location was not accepted): do nothing (keep listening for a better location)
			}
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
	
	@Override
	protected void vibrate(int durationMS)
	{
		DeviceControl.vibrate(activity, durationMS);
	}
	
	@Override
	protected void playSound(File soundFile)
	{
		DeviceControl.playSoundFile(activity, soundFile);
	}

	@Override
	protected void exitApp()
	{		
		activity.finish();
	}

	@Override
	protected void showError(String errorMsg, boolean exit)
	{
		activity.showErrorDialog(errorMsg, exit);
	}

	@Override
	protected void setupTrigger(Trigger trigger)
	{
		// Key press trigger:
		if(!trigger.getKeys().isEmpty())
		{
			activity.setupKeyPressTrigger(trigger);
			addLogLine("TRIGGER", "Set-up key press trigger, firing on pressing of " + CollectionUtils.allToString(trigger.getKeys(), false));
		}		
		// Fixed timer trigger:
		if(trigger.getFixedTimer() != Trigger.NO_TIMEOUT)
		{
			if(logger != null)
				logger.addLine("TRIGGER", "Set-up fixed timer trigger, firing in " + trigger.getFixedTimer() + " seconds");			
			activity.setupTimerTrigger(trigger);
		}
	}
	
	@Override
	protected void disableTrigger(Trigger trigger)
	{
		// Key press trigger:
		if(!trigger.getKeys().isEmpty())
		{
			activity.disableKeyPressTrigger(trigger);
			addLogLine("TRIGGER", "Disabled key press trigger, firing on pressing of " + CollectionUtils.allToString(trigger.getKeys(), false));
		}
		// Fixed timer trigger:
		if(trigger.getFixedTimer() != Trigger.NO_TIMEOUT)
		{
			if(logger != null)
				logger.addLine("TRIGGER", "Disabled fixed timer trigger");
			activity.disableTimerTrigger(trigger);
		}	
	}
	
}
