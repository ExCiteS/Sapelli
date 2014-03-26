package uk.ac.ucl.excites.sapelli.storage.types;

import java.text.ParseException;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.DateTimeColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;


/**
 * A pure-Java (i.e. framework independent) Location class.
 * Implemented as a Record subclass.
 * 
 * It holds latitude and longitude as doubles of the location and an integer indicating the source (provider).<br/>
 * Optionally it can also hold altitude (as double), and bearing, speed & accuracy (all as floats), 
 * 
 * @author mstevens
 */
public class Location extends Record
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
			case PROVIDER_UNKNOWN : return "Unkwown";
			case PROVIDER_GPS : return "GPS";
			case PROVIDER_NETWORK : return "Network";
			case PROVIDER_MANUAL : return "Manual";
			default : return "Unknown";
		}
	}
	
	// Schema(s) & columns
	//	Default Schema (used for Location instances), which uses 64 bit floats (doubles) for latitude, longitude & altitude:
	static final public Schema SCHEMA = new Schema(Schema.ReservedIDs.LOCATION_SCHEMA.ordinal(), Schema.ReservedIDs.LOCATION_SCHEMA.name());
	static final public FloatColumn COLUMN_LATITUDE = new FloatColumn("Latitude", false, true);					// non-optional 64 bit float
	static final public FloatColumn COLUMN_LONGITUDE = new FloatColumn("Longitude", false, true);				// non-optional 64 bit float
	static final public FloatColumn COLUMN_ALTITUDE = new FloatColumn("Altitude", true, true);					// optional 64 bit float
	static final public FloatColumn COLUMN_BEARING = new FloatColumn("Bearing", true, false);					// optional 32 bit float
	static final public FloatColumn COLUMN_SPEED = new FloatColumn("Speed", true, false);						// optional 32 bit float
	static final public FloatColumn COLUMN_ACCURACY = new FloatColumn("Accuracy", true, false);					// optional 32 bit float
	static final public DateTimeColumn COLUMN_TIME = DateTimeColumn.JavaMSTime("Time", true);					// optional 64 bit millisecond timestamp
	static final public IntegerColumn COLUMN_PROVIDER = new IntegerColumn("Provider", false, PROVIDER_FIELD);	// non-optional 2 bit unsigned integer
	static
	{	// Add columns to default Schema & seal it:
		SCHEMA.addColumn(COLUMN_LATITUDE);
		SCHEMA.addColumn(COLUMN_LONGITUDE);
		SCHEMA.addColumn(COLUMN_ALTITUDE);
		SCHEMA.addColumn(COLUMN_BEARING);
		SCHEMA.addColumn(COLUMN_SPEED);
		SCHEMA.addColumn(COLUMN_ACCURACY);
		SCHEMA.addColumn(COLUMN_TIME);
		SCHEMA.addColumn(COLUMN_PROVIDER);
		SCHEMA.seal();
	}
	
	//Dynamic--------------------------------------------------------
	
	/**
	 * @param lat
	 * @param lon
	 */
	public Location(double lat, double lon)
	{
		this(lat, lon, null, null, null, null, (DateTime) null, PROVIDER_UNKNOWN);
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
		this(lat, lon, alt, bearing, speed, acc, (time != null ? new DateTime(time) : null), provider);
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
	public Location(double lat, double lon, Double alt, Float bearing, Float speed, Float acc, DateTime time, int provider)
	{
		super(SCHEMA);
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
	 * Only to be used by  {@link LocationColumn#getNewRecord()}
	 */
	public Location()
	{
		super(SCHEMA);
		COLUMN_LATITUDE.storeValue(this, 0.0d);
		COLUMN_LONGITUDE.storeValue(this, 0.0d);
		COLUMN_PROVIDER.storeValue(this, PROVIDER_UNKNOWN);
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
	public DateTime getTime()
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
	 * 	- v2.x format:					LAT;LON;ALT;BEAR;SPD;ACC;TIME;PROV
	 * 
	 * @param valueString to parse
	 * @return parsed Location object
	 * @throws ParseException
	 */
	public static Location parseV1X(String valueString) throws ParseException
	{
		// Null & empty check:
		if(valueString == null || valueString.isEmpty())
			return null;
		
		// Split up in parts:
		String[] parts = valueString.trim().split("\\" + V1X_SEPARATOR);
		if(parts.length < 3)
			throw new ParseException("Not a valid v1.x location: " + valueString, 0);
		
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
							(parts.length > 7 && !parts[7].isEmpty() ? Long.valueOf(parts[7]) : null),
							Integer.parseInt(parts[oldFormat ? 2 : 0]));
	}
	
}
