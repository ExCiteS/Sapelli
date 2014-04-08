/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Location;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for {@link Location}s, implemented as a {@link RecordColumn} subclass.
 * 
 * @author mstevens
 */
public class LocationColumn extends RecordColumn<Location>
{

	//Static---------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	//	Alternative latitude, longitude & altitude columns using 32 instead of 64 bits (used when doublePrecision=false):
	static final private FloatColumn COLUMN_LATITUDE_32 = new FloatColumn(Location.COLUMN_LATITUDE.getName(), false, true, false);		// non-optional signed 32 bit float
	static final private FloatColumn COLUMN_LONGITUDE_32 = new FloatColumn(Location.COLUMN_LONGITUDE.getName(), false, true, false);	// non-optional signed 32 bit float
	static final private FloatColumn COLUMN_ALTITUDE_32 = new FloatColumn(Location.COLUMN_ALTITUDE.getName(), true, true, false);		// optional signed 32 bit float
		
	//Dynamic--------------------------------------------------------
	
	/**
	 * @param name
	 * @param optional
	 * @param doublePrecision whether or not to store lat/lon/alt as 64 bit (true) or 32 bit (false) values, this only affects binary storage, 64 bits values are used anywhere else
	 * @param storeAltitude
	 * @param storeBearing
	 * @param storeSpeed
	 * @param storeAccuracy
	 * @param storeTime
	 * @param storeProvider
	 */
	public LocationColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeBearing, boolean storeSpeed, boolean storeAccuracy, boolean storeTime, boolean storeProvider)
	{
		super(Location.class, name, Location.SCHEMA, optional);
		// "Skip columns": skip the things we don't want to store binary:
		if(!storeAltitude)
			addSkipColumn(Location.COLUMN_ALTITUDE);
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
		if(!doublePrecision)
		{	// Use 32 bit float columns for binary storage of lat, lon & alt values:
			addBinaryColumn(Location.COLUMN_LATITUDE, COLUMN_LATITUDE_32);
			addBinaryColumn(Location.COLUMN_LONGITUDE, COLUMN_LONGITUDE_32);
			addBinaryColumn(Location.COLUMN_ALTITUDE, COLUMN_ALTITUDE_32);
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
	public Location getNewRecord()
	{
		return new Location();
	}
	
	public boolean isDoublePrecision()
	{
		return getBinaryColumn(Location.COLUMN_LATITUDE) == Location.COLUMN_LATITUDE; // (and not == COLUMN_LATITUDE_32)
	}
	
	public boolean isStoreAltitude()
	{
		return !skipColumns.contains(Location.COLUMN_ALTITUDE);
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

	@Override
	public void accept(ColumnVisitor visitor)
	{
		if(visitor.allowLocationSelfTraversal())
			super.accept(visitor, !visitor.skipNonBinarySerialisedLocationSubColumns());
		else
			visitor.visit(this);
	}
	
}
