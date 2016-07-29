/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

import java.text.ParseException;

import org.joda.time.DateTimeZone;

import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;


/**
 * A pure-Java (i.e. framework independent) Location class.
 * Implemented as a Record subclass.
 * 
 * It holds latitude and longitude as doubles of the location and an integer indicating the source (provider).<br/>
 * Optionally it can also hold altitude (as double), and bearing, speed & accuracy (all as floats), 
 * 
 * @author mstevens
 */
public class Location extends ValueSet<ColumnSet>
{

	//Static---------------------------------------------------------
	static private final long serialVersionUID = 2L;

	static final private char V1X_SEPARATOR = ';';
	
	// Provider
	static public final int PROVIDER_UNKNOWN = 0;
	static public final int PROVIDER_GPS = 1;
	static public final int PROVIDER_NETWORK = 2;
	static public final int PROVIDER_MANUAL = 3; //e.g. pin-pointed on map, coordinates entered in text fields, etc.
	static public final IntegerRangeMapping PROVIDER_FIELD = new IntegerRangeMapping(PROVIDER_UNKNOWN, PROVIDER_MANUAL);
	
	static public String GetProviderName(int provider)
	{
		switch(provider)
		{
			case PROVIDER_GPS :
				return "GPS";
			case PROVIDER_NETWORK :
				return "Network";
			case PROVIDER_MANUAL :
				return "Manual";
			case PROVIDER_UNKNOWN :
			default :
				return "Unknown";
		}
	}
	
	// ColumnSet & Columns:
	static final public ColumnSet COLUMN_SET = new ColumnSet(Location.class.getSimpleName(), false);
	// LATITUDE (non-optional signed 64 bit float):
	static final public FloatColumn		COLUMN_LATITUDE		= COLUMN_SET.addColumn(new FloatColumn("Latitude", false, true, true));
	// LONGITUDE (non-optional signed 64 bit float):
	static final public FloatColumn		COLUMN_LONGITUDE	= COLUMN_SET.addColumn(new FloatColumn("Longitude", false, true, true));
	// ALTITUDE (optional signed 64 bit float):
	static final public FloatColumn		COLUMN_ALTITUDE		= COLUMN_SET.addColumn(new FloatColumn("Altitude", true, true, true));
	// BEARING (optional signed 32 bit float):
	static final public FloatColumn		COLUMN_BEARING		= COLUMN_SET.addColumn(new FloatColumn("Bearing", true, true, false));
	// SPEED (optional signed 32 bit float):
	static final public FloatColumn		COLUMN_SPEED		= COLUMN_SET.addColumn(new FloatColumn("Speed", true, true, false));
	// ACCURACY (optional signed 32 bit float):
	static final public FloatColumn		COLUMN_ACCURACY		= COLUMN_SET.addColumn(new FloatColumn("Accuracy", true, true, false));
	// TIMESTAMP (optional signed 64 bit millisecond-accurate UTC timestamp; local timezone not kept; no virtual columns added):
	static final public TimeStampColumn	COLUMN_TIME			= COLUMN_SET.addColumn(TimeStampColumn.JavaMSTime("TimeUTC", true, false)); 
	// PROVIDER (non-optional 2 bit unsigned integer; with default value = PROVIDER_UNKNOWN):
	static final public IntegerColumn	COLUMN_PROVIDER		= COLUMN_SET.addColumn(new IntegerColumn("Provider", false, PROVIDER_FIELD, Long.valueOf(PROVIDER_UNKNOWN) /*default val*/), true /*seal!*/);
	
	//Dynamic--------------------------------------------------------
	
	/**
	 * @param lat
	 * @param lon
	 */
	public Location(double lat, double lon)
	{
		this(lat, lon, null, null, null, null, (TimeStamp) null, PROVIDER_UNKNOWN);
	}
	
	/**
	 * @param lat
	 * @param lon
	 * @param alt
	 * @param bearing
	 * @param speed
	 * @param acc
	 * @param time
	 * @param provider
	 */
	public Location(double lat, double lon, Double alt, Float bearing, Float speed, Float acc, Long time, int provider)
	{
		this(lat, lon, alt, bearing, speed, acc, (time != null ? new TimeStamp(time) : null), provider);
	}
	
	/**
	 * @param lat
	 * @param lon
	 * @param alt
	 * @param bearing
	 * @param speed
	 * @param acc
	 * @param time
	 * @param provider
	 */
	public Location(double lat, double lon, Double alt, Float bearing, Float speed, Float acc, TimeStamp time, int provider)
	{
		super(COLUMN_SET);
		COLUMN_LATITUDE.storeValue(this, lat);
		COLUMN_LONGITUDE.storeValue(this, lon);
		COLUMN_ALTITUDE.storeValue(this, alt);
		COLUMN_BEARING.storeValue(this, bearing);
		COLUMN_SPEED.storeValue(this, speed);
		COLUMN_ACCURACY.storeValue(this, acc);
		COLUMN_TIME.storeValue(this, time);
		COLUMN_PROVIDER.storeValue(this, provider);
	}
	
	/**
	 * Only to be used by {@link LocationColumn#getNewValueSet()}
	 */
	/*package*/ Location()
	{
		super(COLUMN_SET);
		// no default lat & lon values!
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param another
	 */
	public Location(Location another)
	{
		super(another);
	}
	
	/**
	 * @return the lat
	 */
	public double getLatitude()
	{
		// no need for null check here, the column is non-optional and the value is always set at construction time
		return COLUMN_LATITUDE.retrieveValue(this).doubleValue();
	}
	/**
	 * @return the lon
	 */
	public double getLongitude()
	{
		// no need for null check here, the column is non-optional and the value is always set at construction time
		return COLUMN_LONGITUDE.retrieveValue(this).doubleValue();
	}
	
	public boolean hasAltitude()
	{
		return getValue(COLUMN_ALTITUDE) != null;
	}
	
	/**
	 * @return the alt
	 */
	public double getAltitude()
	{
		return COLUMN_ALTITUDE.getPrimitiveDouble(this, 0.0d);
	}
	
	public boolean hasBearing()
	{
		return getValue(COLUMN_BEARING) != null;
	}
	
	/**
	 * @return the bearing
	 * 
	 * Bearing is the horizontal direction of travel of this device, and is NOT related to the device orientation.
	 */
	public float getBearing()
	{
		return COLUMN_BEARING.getPrimitiveFloat(this, 0.0f);
	}
	
	public boolean hasSpeed()
	{
		return getValue(COLUMN_SPEED) != null;
	}
	
	/**
	 * @return the bearing
	 * 
	 * In meters/second
	 */
	public float getSpeed()
	{
		return COLUMN_SPEED.getPrimitiveFloat(this, 0.0f);
	}
	
	public boolean hasAccuracy()
	{
		return getValue(COLUMN_ACCURACY) != null;
	}
	
	/**
	 * @return the acc
	 */
	public float getAccuracy()
	{
		return COLUMN_ACCURACY.getPrimitiveFloat(this, 0.0f);
	}
	
	public boolean hasTime()
	{
		return getValue(COLUMN_TIME) != null;
	}

	/**
	 * @return the time
	 */
	public TimeStamp getTime()
	{
		return COLUMN_TIME.retrieveValue(this);
	}
	
	/**
	 * @return the provider
	 */
	public int getProvider()
	{
		// no need for null check here, the column is non-optional and the value is always set at construction time
		return COLUMN_PROVIDER.retrieveValue(this).intValue();
	}

	/**
	 * This method supports parsing Locations from 2 v1.x formats, one from before and
	 * one from after 2013-07-13.
	 * 
	 * Formats overview:
	 * 	- Pre-2013-07-13 v1.x format:	LAT;LON;PROV;ALT;BEAR;SPD;ACC;TIME
	 * 	- Post-2013-07-13 v1.x format:	PROV;LAT;LON;ALT;BEAR;SPD;ACC;TIME
	 * 	- v2.x format:					LAT,LON,ALT,BEAR,SPD,ACC,TIME,PROV
	 * 
	 * @param valueString the {@link String} to parse, may be {@code null} or empty
	 * @return parsed Location object, or {@code null} if valueString was {@code null} or empty
	 * @throws ParseException
	 */
	public static Location parseV1X(String valueString) throws ParseException
	{
		// Null & empty check:
		if(valueString == null || valueString.isEmpty())
			return null;
		else
			valueString = valueString.trim();
		
		// Split up in parts:
		String[] parts = valueString.split("\\" + V1X_SEPARATOR, -1); // -1: allow empty Strings
		if(parts.length < 3)
			throw new ParseException("Not a valid v1.x Location value: " + valueString, 0);
		
		//Check for Pre-2013-07-13 old serialisation format:
		boolean oldFormat = false;
		if(parts[0].indexOf('.') != -1) //first part is a floating point value (i.e. it is lat, as in the old format)
			oldFormat = true;
		
		// Construct new location object:
		return new Location(Double.parseDouble(parts[oldFormat ? 0 : 1]),
							Double.parseDouble(parts[oldFormat ? 1 : 2]),
							(parts.length > 3 && !parts[3].isEmpty() ? Double.valueOf(parts[3]) : null),
							(parts.length > 4 && !parts[4].isEmpty() ? Float.valueOf(parts[4]) : null),
							(parts.length > 5 && !parts[5].isEmpty() ? Float.valueOf(parts[5]) : null),
							(parts.length > 6 && !parts[6].isEmpty() ? Float.valueOf(parts[6]) : null),
							(parts.length > 7 && !parts[7].isEmpty() ? new TimeStamp(Long.valueOf(parts[7]), DateTimeZone.UTC) : null),
							Integer.parseInt(parts[oldFormat ? 2 : 0]));
	}
	
}
