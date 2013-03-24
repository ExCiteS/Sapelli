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
	static final private String SEPARATOR = "|";
	
	static public final int PROVIDER_UNKNOWN = 0;
	static public final int PROVIDER_GPS = 1;
	static public final int PROVIDER_NETWORK = 2;
	static public final int PROVIDER_MANUAL = 3; //e.g. pinpointed on map, coordinates entered in text fields, etc.
	
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
	
	static public Location Parse(String text) throws ParseException
	{
		String[] parts = text.trim().split("\\" + SEPARATOR);
		if(parts.length < 3)
			throw new ParseException("Not a valid location: " + text, 0);
		return new Location(Double.parseDouble(parts[0]),
							Double.parseDouble(parts[1]),
							Integer.parseInt(parts[2]),
							(parts.length > 3 && !parts[3].isEmpty() ? Double.valueOf(parts[3]) : null),
							(parts.length > 4 && !parts[4].isEmpty() ? Float.valueOf(parts[4]) : null),
							(parts.length > 5 && !parts[5].isEmpty() ? Float.valueOf(parts[5]) : null),
							(parts.length > 6 && !parts[6].isEmpty() ? Float.valueOf(parts[6]) : null),
							(parts.length > 7 && !parts[7].isEmpty() ? Long.valueOf(parts[7]) : null));
	}
	
	//Dynamic--------------------------------------------------------
	protected double lat;
	protected double lon;
	protected int provider;
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
		this(lat, lon, PROVIDER_UNKNOWN, null, null, null, null, null);
	}
	
	/**
	 * @param lat
	 * @param lon
	 * @param provider
	 * @param alt
	 * @param bearing
	 * @param speed
	 * @param acc
	 * @param time
	 */
	public Location(double lat, double lon, int provider, Double alt, Float bearing, Float speed, Float acc, Long time)
	{
		this.lat = lat;
		this.lon = lon;
		this.provider = provider;
		//optional (these can be null):
		this.alt = alt;
		this.bearing = bearing;
		this.speed = speed;
		this.acc = acc;
		this.time = time;
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
	
	/**
	 * @return the provider
	 */
	public int getProvider()
	{
		return provider;
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

	public String toString()
	{
		return 	Double.toString(lat) + SEPARATOR +
				Double.toString(lon) + SEPARATOR +
				Integer.toString(provider) + SEPARATOR +
				(alt != null ? alt.toString() : "") + SEPARATOR +
				(bearing != null ? bearing.toString() : "") + SEPARATOR +
				(speed != null ? speed.toString() : "") + SEPARATOR +
				(acc != null ? acc.toString() : "") + SEPARATOR +
				(time != null ? time.toString() : "");
	}
	
}
