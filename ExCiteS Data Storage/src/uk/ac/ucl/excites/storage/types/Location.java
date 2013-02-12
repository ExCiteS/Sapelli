package uk.ac.ucl.excites.storage.types;

import org.apache.http.ParseException;


/**
 * A pure-Java (i.e. framework independent) Location class<br/>
 * 
 * It holds latitude and longitude as 64 bit doubles.<br/>
 * Optionally it can also hold altitude (as double) and bearing, speed & accuracy (all as 32 bit floats).
 * 
 * @author mstevens
 */
public class Location
{

	//Static---------------------------------------------------------
	static final private String SEPARATOR = "|";
	
	static public Location Parse(String text) throws ParseException
	{
		String[] parts = text.trim().split("\\" + SEPARATOR);
		if(parts.length < 2)
			throw new ParseException("Not a value location: " + text);
		return new Location(Double.parseDouble(parts[0]),
							Double.parseDouble(parts[1]),
							(parts.length > 2 && !parts[2].isEmpty() ? Double.valueOf(parts[2]) : null),
							(parts.length > 3 && !parts[3].isEmpty() ? Float.valueOf(parts[3]) : null),
							(parts.length > 4 && !parts[4].isEmpty() ? Float.valueOf(parts[4]) : null),
							(parts.length > 5 && !parts[5].isEmpty() ? Float.valueOf(parts[5]) : null));
	}
	
	//Dynamic--------------------------------------------------------
	protected double lat;
	protected double lon;
	protected Double alt;
	protected Float bearing;
	protected Float speed;
	protected Float acc;
	
	/**
	 * @param lat
	 * @param lon
	 */
	public Location(double lat, double lon)
	{
		this(lat, lon, null, null, null, null);
	}
	
	/**
	 * @param lat
	 * @param lon
	 * @param alt
	 * @param bearing
	 * @param speed
	 * @param acc
	 */
	public Location(double lat, double lon, Double alt, Float bearing, Float speed, Float acc)
	{
		this.lat = lat;
		this.lon = lon;
		this.alt = alt;
		this.bearing = bearing;
		this.speed = speed;
		this.acc = acc;
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
	
	public String toString()
	{
		return 	Double.toString(lat) + SEPARATOR +
				Double.toString(lon) + SEPARATOR +
				(alt != null ? alt.toString() : "") + SEPARATOR +
				(bearing != null ? bearing.toString() : "") + SEPARATOR +
				(speed != null ? speed.toString() : "") + SEPARATOR +
				(acc != null ? acc.toString() : "");
	}
	
}
