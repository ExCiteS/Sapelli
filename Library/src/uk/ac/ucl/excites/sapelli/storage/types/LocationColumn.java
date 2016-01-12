/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.storage.types;

import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for {@link Location}s, implemented as a {@link ValueSetColumn} subclass.
 * 
 * @author mstevens
 */
public class LocationColumn extends ValueSetColumn<Location, ColumnSet>
{

	//Static---------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	//	Alternative latitude, longitude & altitude columns using 32 instead of 64 bits (used when doublePrecision=false):
	static final private FloatColumn COLUMN_LATITUDE_32 = new FloatColumn(Location.COLUMN_LATITUDE.getName(), false, true, false);		// non-optional signed 32 bit float (lossy)
	static final private FloatColumn COLUMN_LONGITUDE_32 = new FloatColumn(Location.COLUMN_LONGITUDE.getName(), false, true, false);	// non-optional signed 32 bit float (lossy)
	static final private FloatColumn COLUMN_ALTITUDE_32 = new FloatColumn(Location.COLUMN_ALTITUDE.getName(), true, true, false);		// optional signed 32 bit float (lossy)
		
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
		this(name, optional, doublePrecision, storeAltitude, storeBearing, storeSpeed, storeAccuracy, storeTime, storeProvider, null);
	}
	
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
	 * @param defaultValue
	 */
	public LocationColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeBearing, boolean storeSpeed, boolean storeAccuracy, boolean storeTime, boolean storeProvider, Location defaultValue)
	{
		super(name, Location.COLUMN_SET, optional, defaultValue);
		// "Skip columns": skip the things we don't want to store binary:
		if(!storeAltitude)
			skipColumn(Location.COLUMN_ALTITUDE);
		if(!storeBearing)
			skipColumn(Location.COLUMN_BEARING);
		if(!storeSpeed)
			skipColumn(Location.COLUMN_SPEED);
		if(!storeAccuracy)
			skipColumn(Location.COLUMN_ACCURACY);
		if(!storeTime)
			skipColumn(Location.COLUMN_TIME);
		if(!storeProvider)
			skipColumn(Location.COLUMN_PROVIDER);
		if(!doublePrecision)
		{	// Use 32 bit float columns for (lossy) binary storage of lat, lon & alt values:
			swapColumn(Location.COLUMN_LATITUDE, COLUMN_LATITUDE_32);
			swapColumn(Location.COLUMN_LONGITUDE, COLUMN_LONGITUDE_32);
			swapColumn(Location.COLUMN_ALTITUDE, COLUMN_ALTITUDE_32);
		}
	}

	@Override
	protected LocationColumn createCopy()
	{
		return new LocationColumn(	name,
									optional,
									isDoublePrecision(),
									isStoreAltitude(),
									isStoreBearing(),
									isStoreSpeed(),
									isStoreAccuracy(),
									isStoreTime(),
									isStoreProvider(),
									defaultValue);
	}
	
	@Override
	public Location getNewValueSet()
	{
		return new Location();
	}
	
	public boolean isDoublePrecision()
	{
		return getBinaryColumn(Location.COLUMN_LATITUDE) == Location.COLUMN_LATITUDE; // (and not == COLUMN_LATITUDE_32)
	}
	
	public boolean isStoreAltitude()
	{
		return !isColumnSkipped(Location.COLUMN_ALTITUDE);
	}
	
	public boolean isStoreBearing()
	{
		return !isColumnSkipped(Location.COLUMN_BEARING);
	}
	
	public boolean isStoreSpeed()
	{
		return !isColumnSkipped(Location.COLUMN_SPEED);
	}
	
	public boolean isStoreAccuracy()
	{
		return !isColumnSkipped(Location.COLUMN_ACCURACY);
	}
	
	public boolean isStoreTime()
	{
		return !isColumnSkipped(Location.COLUMN_TIME);
	}
	
	public boolean isStoreProvider()
	{
		return !isColumnSkipped(Location.COLUMN_PROVIDER);
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
		if(visitor.splitLocationTraversal())
			super.accept(visitor, !visitor.skipNonBinarySerialisedLocationSubColumns()); // visit as ValueSetColumn: enter event, visit or each subcolumn, leave event
		else
			visitor.visit(this); // visit as LocationColumn (as a single whole)
	}

	@Override
	public Class<Location> getType()
	{
		return Location.class;
	}
	
}
