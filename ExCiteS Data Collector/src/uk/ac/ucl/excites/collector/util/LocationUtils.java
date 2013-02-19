package uk.ac.ucl.excites.collector.util;

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
		return new Location(androidLocation.getLatitude(),
							androidLocation.getLongitude(),
							(androidLocation.hasAltitude() ? androidLocation.getAltitude() : null),
							(androidLocation.hasBearing() ? androidLocation.getBearing() : null),
							(androidLocation.hasSpeed() ? androidLocation.getSpeed() : null),
							(androidLocation.hasAccuracy() ? androidLocation.getAccuracy() : null));
	}
	
}
