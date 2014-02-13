package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.location.LocationManager;
import uk.ac.ucl.excites.sapelli.collector.project.model.LocationField;
import uk.ac.ucl.excites.sapelli.storage.types.Location;

/**
 * @author mstevens
 * 
 */
public final class LocationUtils
{

	private static final int HUNDRED_METERS = 100;
	private static final int NINETY_SECONDS_IN_MS = 90 * 1000;

	private LocationUtils()
	{
	}

	/**
	 * Converts an Android Location object (android.location.Location) into an equivalent "ExCiteS" location object (uk.ac.ucl.excites.sapelli.storage.types.Location)
	 * 
	 * @param androidLocation
	 * @return
	 */
	static public Location getExCiteSLocation(android.location.Location androidLocation)
	{
		if(androidLocation == null)
			return null;
		int provider = Location.PROVIDER_UNKNOWN;
		if(LocationManager.GPS_PROVIDER.equals(androidLocation.getProvider()))
			provider = Location.PROVIDER_GPS;
		else if(LocationManager.NETWORK_PROVIDER.equals(androidLocation.getProvider()))
			provider = Location.PROVIDER_NETWORK;
		return new Location(provider,
							androidLocation.getLatitude(),
							androidLocation.getLongitude(),
							(androidLocation.hasAltitude() ? androidLocation.getAltitude() : null),
							(androidLocation.hasBearing() ? androidLocation.getBearing() : null),
							(androidLocation.hasSpeed() ? androidLocation.getSpeed() : null),
							(androidLocation.hasAccuracy() ? androidLocation.getAccuracy() : null),
							androidLocation.getTime());
	}

	static public List<String> getProvider(LocationManager locManager, LocationField locField)
	{
		switch(locField.getType())
		{
			case LocationField.TYPE_ANY:
				return locManager.getAllProviders();
			case LocationField.TYPE_GPS:
				return Arrays.asList(LocationManager.GPS_PROVIDER);
			case LocationField.TYPE_NETWORK:
				return Arrays.asList(LocationManager.NETWORK_PROVIDER);
				// others later?
		}
		return new ArrayList<String>();
	}

	/**
	 * Determines whether one Location reading is better than the current Location fix.<br/>
	 * Adapted from <a href="http://developer.android.com/guide/topics/location/strategies.html#BestEstimate">http://developer.android.com/guide/topics/location/strategies.html#BestEstimate</a>.
	 * 
	 * @param location
	 *            The new android.location.Location that you want to evaluate
	 * @param currentBestLocation
	 *            The current android.location.Location fix, to which you want to compare the new one
	 *            
	 * @see <a href="http://developer.android.com/guide/topics/location/strategies.html#BestEstimate">http://developer.android.com/guide/topics/location/strategies.html#BestEstimate</a>
	 */
	static public boolean isBetterLocation(android.location.Location location, android.location.Location currentBestLocation)
	{
		if(currentBestLocation == null)
			return true; //A new location is always better than no location

		//Check whether the new location fix is newer or older
		long timeDelta = location.getTime() - currentBestLocation.getTime();
		boolean isSignificantlyNewer = timeDelta > NINETY_SECONDS_IN_MS;
		boolean isSignificantlyOlder = timeDelta < -NINETY_SECONDS_IN_MS;
		boolean isNewer = timeDelta > 0;

		if(isSignificantlyNewer)
			return true; //If it's been more than two minutes since the current location, use the new location because the user has likely moved
		else if(isSignificantlyOlder)
			return false; //If the new location is more than two minutes older, it must be worse

		//Check whether the new location fix is more or less accurate
		int accuracyDelta = (int) (location.getAccuracy() - currentBestLocation.getAccuracy());
		boolean isLessAccurate = accuracyDelta > 0;
		boolean isMoreAccurate = accuracyDelta < 0;
		boolean isSignificantlyLessAccurate = accuracyDelta > HUNDRED_METERS;

		//Check if the old and new location are from the same provider
		boolean isFromSameProvider = isSameProvider(location.getProvider(), currentBestLocation.getProvider());

		//Determine location quality using a combination of timeliness and accuracy
		if(isMoreAccurate)
			return true;
		else if(isNewer && !isLessAccurate)
			return true;
		else if(isNewer && !isSignificantlyLessAccurate && isFromSameProvider)
			return true;
		return false;
	}

	/** Checks whether two providers are the same */
	static public boolean isSameProvider(String provider1, String provider2)
	{
		if(provider1 == null)
			return provider2 == null;
		return provider1.equals(provider2);
	}

}
