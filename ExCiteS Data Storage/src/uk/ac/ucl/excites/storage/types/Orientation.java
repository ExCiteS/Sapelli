package uk.ac.ucl.excites.storage.types;

import org.apache.http.ParseException;

/**
 * @author mstevens
 *
 */
public class Orientation
{
	
	//Statics----------------------------------------------
	static final private String SEPARATOR = "|";
	
	static public Orientation Parse(String text)
	{
		String[] parts = text.trim().split("\\" + SEPARATOR);
		if(parts.length != 3)
			throw new ParseException("Not a valid orientation: " + text);
		return new Orientation(	(!parts[0].isEmpty() ? Float.valueOf(parts[0]) : null),
								(!parts[1].isEmpty() ? Float.valueOf(parts[1]) : null),
								(!parts[2].isEmpty() ? Float.valueOf(parts[2]) : null));
	}
	
	//Dynamics---------------------------------------------
	private Float x;
	private Float y;
	private Float z;
	
	/**
	 * @param x
	 * @param y
	 * @param z
	 */
	public Orientation(Float x, Float y, Float z)
	{
		this.x = x;
		this.y = y;
		this.z = z;
	}

	/**
	 * @return the x
	 */
	public float getX()
	{
		if(x == null)
			return 0.0f;
		return x;
	}

	public boolean hasX()
	{
		return x != null;
	}
	
	/**
	 * @return the y
	 */
	public float getY()
	{
		if(x == null)
			return 0.0f;
		return y;
	}

	public boolean hasY()
	{
		return y != null;
	}
	
	/**
	 * @return the z
	 */
	public float getZ()
	{
		if(x == null)
			return 0.0f;
		return z;
	}
	
	public boolean hasZ()
	{
		return z != null;
	}
	
	public String toString()
	{
		return	(x != null ? x.toString() : "") + SEPARATOR +
				(y != null ? y.toString() : "") + SEPARATOR +
				(z != null ? z.toString() : "") + SEPARATOR;
	}
	
}
