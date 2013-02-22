package uk.ac.ucl.excites.collector.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.location.LocationManager;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.storage.types.Location;

/**
 * @author mstevens
 *
 */
public final class LocationUtils
{

	private LocationUtils() {}
	
	/**
	 * Converts an Android Location object (android.location.Location) into an equivalent "ExCiteS" location object (uk.ac.ucl.excites.storage.types.Location)
	 * 
	 * @param androidLocation
	 * @return
	 */
	static public Location getExCiteSLocation(android.location.Location androidLocation)
	{
		int provider = Location.PROVIDER_UNKNOWN;
		if(LocationManager.GPS_PROVIDER.equals(androidLocation.getProvider()))
			provider = Location.PROVIDER_GPS;
		else if(LocationManager.NETWORK_PROVIDER.equals(androidLocation.getProvider()))
			provider = Location.PROVIDER_NETWORK;
		return new Location(androidLocation.getLatitude(),
							androidLocation.getLongitude(),
							provider,
							(androidLocation.hasAltitude() ? androidLocation.getAltitude() : null),
							(androidLocation.hasBearing() ? androidLocation.getBearing() : null),
							(androidLocation.hasSpeed() ? androidLocation.getSpeed() : null),
							(androidLocation.hasAccuracy() ? androidLocation.getAccuracy() : null));
	}
	
	static public List<String> getProvider(LocationManager locManager, LocationField locField)
	{
		switch(locField.getType())
		{
			case LocationField.TYPE_ANY : return locManager.getAllProviders();	
			case LocationField.TYPE_GPS : return Arrays.asList(LocationManager.GPS_PROVIDER);
			case LocationField.TYPE_NETWORK : return Arrays.asList(LocationManager.NETWORK_PROVIDER);
			//others later?
		}
		return new ArrayList<String>();
	}
	
}
