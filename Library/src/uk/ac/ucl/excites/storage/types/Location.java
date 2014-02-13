package uk.ac.ucl.excites.storage.types;

import java.text.ParseException;

import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;


/**
 * A pure-Java (i.e. framework independent) Location class<br/>
 * 
 * It holds latitude and longitude as 64 bit doubles, and an interger indicating the source (providor) of the location.<br/>
 * Optionally it can also hold altitude (as double) and bearing, speed & accuracy (all as 32 bit floats).
 * 
 * @author mstevens
 */
public class Location
{

	//Static---------------------------------------------------------
	static final private String SEPARATOR = ";";
	
	static public final int PROVIDER_UNKNOWN = 0;
	static public final int PROVIDER_GPS = 1;
	static public final int PROVIDER_NETWORK = 2;
	static public final int PROVIDER_MANUAL = 3; //e.g. pin-pointed on map, coordinates entered in text fields, etc.
	
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
	
	static public IntegerRangeMapping ProviderRange()
	{
		return new IntegerRangeMapping(PROVIDER_UNKNOWN, PROVIDER_MANUAL);
	}
	
	/**
	 * Warning (2013-07-13): Serialisation format produced by {@link Location#toString()} changed!<br/>
	 * &nbsp;&nbsp;&nbsp; provider moved to front, whereas it used to come between lon and alt.<br/>
	 * &nbsp;&nbsp;&nbsp; Old format is still supported for parsing.
	 * 
	 * @param text to parse
	 * @return parsed Location object
	 * @throws ParseException
	 */
	static public Location Parse(String text) throws ParseException
	{
		String[] parts = text.trim().split("\\" + SEPARATOR);
		if(parts.length < 3)
			throw new ParseException("Not a valid location: " + text, 0);
		
		//Check for old serialisation format:
		boolean oldFormat = false;
		if(parts[0].indexOf('.') != -1) //first part is a floating point value (i.e. it is lat, as in the old format)
			oldFormat = true;
		
		return new Location(Integer.parseInt(parts[oldFormat ? 2 : 0]),
							Double.parseDouble(parts[oldFormat ? 0 : 1]),
							Double.parseDouble(parts[oldFormat ? 1 : 2]),
							(parts.length > 3 && !parts[3].isEmpty() ? Double.valueOf(parts[3]) : null),
							(parts.length > 4 && !parts[4].isEmpty() ? Float.valueOf(parts[4]) : null),
							(parts.length > 5 && !parts[5].isEmpty() ? Float.valueOf(parts[5]) : null),
							(parts.length > 6 && !parts[6].isEmpty() ? Float.valueOf(parts[6]) : null),
							(parts.length > 7 && !parts[7].isEmpty() ? Long.valueOf(parts[7]) : null));
	}
	
	//Dynamic--------------------------------------------------------
	protected int provider;
	protected double lat;
	protected double lon;
	protected Double alt;
	protected Float bearing;
	protected Float speed;
	protected Float acc;
	protected Long time;
	
	/**
	 * @param lat
	 * @param lon
	 */
	public Location(double lat, double lon)
	{
		this(PROVIDER_UNKNOWN, lat, lon, null, null, null, null, null);
	}
	
	/**
	 * @param provider
	 * @param lat
	 * @param lon
	 * @param alt
	 * @param bearing
	 * @param speed
	 * @param acc
	 * @param time
	 */
	public Location(int provider, double lat, double lon, Double alt, Float bearing, Float speed, Float acc, Long time)
	{
		this.provider = provider;
		this.lat = lat;
		this.lon = lon;
		//optional (these can be null):
		this.alt = alt;
		this.bearing = bearing;
		this.speed = speed;
		this.acc = acc;
		this.time = time;
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param another
	 */
	public Location(Location another)
	{
		this.provider = another.provider;
		this.lat = another.lat;
		this.lon = another.lon;
		this.alt = another.alt; 
		this.bearing = another.bearing;
		this.speed = another.speed;
		this.acc = another.acc;
		this.time = another.time;
	}
	
	/**
	 * @return the provider
	 */
	public int getProvider()
	{
		return provider;
	}
	
	/**
	 * @return the lat
	 */
	public double getLatitude()
	{
		return lat;
	}
	/**
	 * @return the lon
	 */
	public double getLongitude()
	{
		return lon;
	}
	
	public boolean hasAltitude()
	{
		return alt != null;
	}
	
	/**
	 * @return the alt
	 */
	public double getAltitude()
	{
		if(alt == null)
			return 0.0d;
		return alt;
	}
	
	public boolean hasBearing()
	{
		return bearing != null;
	}
	
	/**
	 * @return the bearing
	 * 
	 * Bearing is the horizontal direction of travel of this device, and is NOT related to the device orientation.
	 */
	public float getBearing()
	{
		if(bearing == null)
			return 0.0f;
		return bearing;
	}
	
	public boolean hasSpeed()
	{
		return speed != null;
	}
	
	/**
	 * @return the bearing
	 * 
	 * In meters/second
	 */
	public float getSpeed()
	{
		if(speed == null)
			return 0.0f;
		return speed;
	}
	
	public boolean hasAccuracy()
	{
		return acc != null;
	}
	
	/**
	 * @return the acc
	 */
	public float getAccuracy()
	{
		if(acc == null)
			return 0.0f;
		return acc;
	}
	
	public boolean hasTime()
	{
		return time != null;
	}

	/**
	 * @return the time
	 */
	public long getTime()
	{
		if(time == null)
			return 0l;
		return time;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + provider;
		hash = 31 * hash + Double.valueOf(lat).hashCode();
		hash = 31 * hash + Double.valueOf(lon).hashCode();
		hash = 31 * hash + (alt == null ? 0 : alt.hashCode());
		hash = 31 * hash + (bearing == null ? 0 : bearing.hashCode());
		hash = 31 * hash + (speed == null ? 0 : speed.hashCode());
		hash = 31 * hash + (acc == null ? 0 : acc.hashCode());
		hash = 31 * hash + (time == null ? 0 : time.hashCode());
		return hash;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof Location)
		{
			Location other = (Location) obj;
			return	this.provider == other.provider &&
					this.lat == other.lat &&
					this.lon == other.lon &&
					(this.alt == null ? other.alt == null : this.alt.equals(other.alt)) &&
					(this.bearing == null ? other.bearing == null : this.bearing.equals(other.bearing)) &&
					(this.speed == null ? other.speed == null : this.speed.equals(other.speed)) &&
					(this.acc == null ? other.acc == null : this.acc.equals(other.acc)) &&
					(this.time == null ? other.time == null : this.time.equals(other.time));
		}
		else
			return false;
	}

	/**
	 * Warning (2013-07-13): Serialisation format changed!<br/>
	 * 	&nbsp;&nbsp;&nbsp; provider moved to front, whereas it used to come between lon and alt.<br/>
	 * 	&nbsp;&nbsp;&nbsp; Old format is still supported in {@link Location#Parse(String)}
	 * 
	 * @see java.lang.Object#toString()
	 */
	public String toString()
	{
		return 	Integer.toString(provider) + SEPARATOR +
				Double.toString(lat) + SEPARATOR +
				Double.toString(lon) + SEPARATOR +
				(alt != null ? alt.toString() : "") + SEPARATOR +
				(bearing != null ? bearing.toString() : "") + SEPARATOR +
				(speed != null ? speed.toString() : "") + SEPARATOR +
				(acc != null ? acc.toString() : "") + SEPARATOR +
				(time != null ? time.toString() : "");
	}
	
	static public String GetStringHeader(String prefix, String separator)
	{
		return 	prefix + "provider" + separator +
				prefix + "lat" + separator +
				prefix + "lon" + separator +
				prefix + "alt" + separator +
				prefix + "bearing" + separator +
				prefix + "speed" + separator +
				prefix + "acc" + separator +
				prefix + "time";
	}
	
}
