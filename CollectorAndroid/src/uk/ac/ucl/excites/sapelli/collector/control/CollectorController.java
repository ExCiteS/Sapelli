/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.control;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.activities.CollectorActivity;
import uk.ac.ucl.excites.sapelli.collector.database.DataAccess;
import uk.ac.ucl.excites.sapelli.collector.geo.OrientationListener;
import uk.ac.ucl.excites.sapelli.collector.geo.OrientationSensor;
import uk.ac.ucl.excites.sapelli.collector.project.model.Form.Next;
import uk.ac.ucl.excites.sapelli.collector.project.model.Project;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;
import uk.ac.ucl.excites.sapelli.collector.util.LocationUtils;
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

	// STATICS--------------------------------------------------------
	public static final String TAG = "ProjectController";
	public static final int LOCATION_LISTENER_UPDATE_MIN_TIME_MS = 15 * 1000;// 15 seconds
	public static final int LOCATION_LISTENER_UPDATE_MIN_DISTANCE_M = 5; // 5 meters

	public CollectorActivity activity;

	private LocationManager locationManager;
	private Location currentBestLocation = null;
	private OrientationSensor orientationSensor;

	public CollectorController(Project project, DataAccess dao, CollectorActivity activity)
	{
		super(project, dao);
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
	public boolean enterPhotoField(PhotoField pf)
	{
		if(!enterMediaField(pf))
			return false;
		if(pf.isUseNativeApp())
			activity.startCameraApp(); // Start native camera app of device
		return !pf.isUseNativeApp();
	}

	@Override
	public boolean enterLocationField(LocationField lf)
	{
		if(lf.isWaitAtField() || /*try to use currentBestLocation:*/ !lf.storeLocation(currentRecord, LocationUtils.getSapelliLocation(currentBestLocation)))
		{
			startLocationListener(lf); // start listening for a location
			return true;
		}
		else
		{ // we already have a (good enough) location
			goForward(false); // skip the wait screen
			return false;
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
	protected void saveRecordAndAttachments()
	{	
		super.saveRecordAndAttachments(); //!!!
	
		// Also print the record on Android Log:
		if(currentForm.isProducesRecords())
		{
			Log.d(TAG, "Stored record:");
			Log.d(TAG, currentRecord.toString());
		}
	}

	public void timeout(Field field)
	{
		if(field != currentField)
			return; // this shouldn't happen really
		//Log:
		if(logger != null)
			logger.addLine("TIMEOUT", currentField.getID());
		// Handle location field
		if(currentField instanceof LocationField)
		{
			LocationField lf = (LocationField) currentField;
			if(lf.retrieveLocation(currentRecord) == null && lf.isUseBestNonQualifyingLocationAfterTimeout())
				lf.storeLocation(currentRecord, LocationUtils.getSapelliLocation(currentBestLocation), true);
			
			// If still no location set (because either isUseBestNQLAT==false or currentBestLocation==null), and locationField is non-optional: cancel & exit!
			if(lf.retrieveLocation(currentRecord) == null && lf.getOptional() != Optionalness.ALWAYS)
			{
				activity.runOnUiThread(new Runnable()
				{
					@Override
					public void run()
					{
						activity.showErrorDialog("Cannot get GPS signal and location is mandatory for field '" + currentField.getID() + "'. Please, make sure your GPS receiver is enabled.", true, new Runnable()
						{
							@Override
							public void run()
							{
								goTo(new EndField(currentForm, false, Next.EXITAPP));
							}
						});
					}
				});
				return;
			}
		}
		// else if() //other fields with timeouts in the future?
		// ...
		// Continue:
		goForward(false);
	}

	public void onOrientationChanged(Orientation orientation)
	{
		if(currentField instanceof OrientationField)
		{
			((OrientationField) currentField).storeValue(currentRecord, orientation);
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
			if(currentField instanceof LocationField)
			{ // user is currently waiting for a location for the currentField
				LocationField lf = (LocationField) currentField;
				// try to store location:
				if(lf.storeLocation(currentRecord, LocationUtils.getSapelliLocation(location)))
				{ // location successfully stored:
					if(currentForm.getLocationFields(true).isEmpty())
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
	protected void displayField(Field currentField)
	{
		activity.getCollectorView().setField(currentField);
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
	
}
