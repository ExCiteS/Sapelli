/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Location;

/**
 * 
 * 
 * @author mstevens
 */
public class LocationColumn extends RecordColumn<Location>
{
	
	/**
	 * @param name
	 * @param optional
	 * @param doublePrecision
	 * @param storeAltitude
	 * @param storeBearing
	 * @param storeSpeed
	 * @param storeAccuracy
	 * @param storeTime
	 * @param storeProvider
	 */
	public LocationColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeBearing, boolean storeSpeed, boolean storeAccuracy, boolean storeTime, boolean storeProvider)
	{
		super(Location.class, name, doublePrecision ? Location.SCHEMA : Location.SCHEMA_32, optional);
		// "Skip columns": skip the things we don't want to store binary:
		if(!storeAltitude)
			addSkipColumn(doublePrecision ? Location.COLUMN_ALTITUDE : Location.COLUMN_ALTITUDE_32);
		if(!storeBearing)
			addSkipColumn(Location.COLUMN_BEARING);
		if(!storeSpeed)
			addSkipColumn(Location.COLUMN_SPEED);
		if(!storeAccuracy)
			addSkipColumn(Location.COLUMN_ACCURACY);
		if(!storeTime)
			addSkipColumn(Location.COLUMN_TIME);
		if(!storeProvider)
			addSkipColumn(Location.COLUMN_PROVIDER);
		// "Swap columns": use 64-bit columns when storing/retrieving lat/lon/alt values in the Location objects:
		if(!doublePrecision)
		{
			addRecordColumn(Location.COLUMN_LATITUDE_32, Location.COLUMN_LATITUDE);
			addRecordColumn(Location.COLUMN_LONGITUDE_32, Location.COLUMN_LONGITUDE);
			addRecordColumn(Location.COLUMN_ALTITUDE_32, Location.COLUMN_ALTITUDE);
		}
	}

	@Override
	public LocationColumn copy()
	{
		return new LocationColumn(	name,
									optional,
									isDoublePrecision(),
									isStoreAltitude(),
									isStoreBearing(),
									isStoreSpeed(),
									isStoreAccuracy(),
									isStoreTime(),
									isStoreProvider());
	}
	
	@Override
	protected Location getNewRecord()
	{
		return new Location();
	}
	
	public boolean isDoublePrecision()
	{
		return schema.containsColumn(Location.COLUMN_LATITUDE); // rather than COLUMN_LATITUDE_32
	}
	
	public boolean isStoreAltitude()
	{
		return !skipColumns.contains(isDoublePrecision() ? Location.COLUMN_ALTITUDE : Location.COLUMN_ALTITUDE_32);
	}
	
	public boolean isStoreBearing()
	{
		return !skipColumns.contains(Location.COLUMN_BEARING);
	}
	
	public boolean isStoreSpeed()
	{
		return !skipColumns.contains(Location.COLUMN_SPEED);
	}
	
	public boolean isStoreAccuracy()
	{
		return !skipColumns.contains(Location.COLUMN_ACCURACY);
	}
	
	public boolean isStoreTime()
	{
		return !skipColumns.contains(Location.COLUMN_TIME);
	}
	
	public boolean isStoreProvider()
	{
		return !skipColumns.contains(Location.COLUMN_PROVIDER);
	}

	@Override
	protected void validate(Location value) throws IllegalArgumentException
	{
		//does nothing (for now)
	}

	@Override
	protected Location copy(Location value)
	{
		return new Location(value);
	}

}
