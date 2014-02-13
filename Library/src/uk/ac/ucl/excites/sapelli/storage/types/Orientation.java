package uk.ac.ucl.excites.sapelli.storage.types;

import java.text.ParseException;

/**
 * @author mstevens
 *
 */
public class Orientation
{
	
	//Statics----------------------------------------------
	static final private String SEPARATOR = ";";
	
	static public Orientation Parse(String text) throws ParseException
	{
		String[] parts = text.trim().split("\\" + SEPARATOR);
		if(parts.length != 3)
			throw new ParseException("Not a valid orientation: " + text, 0);
		return new Orientation(	(!parts[0].isEmpty() ? Float.valueOf(parts[0]) : null),
								(!parts[1].isEmpty() ? Float.valueOf(parts[1]) : null),
								(!parts[2].isEmpty() ? Float.valueOf(parts[2]) : null));
	}
	
	//Dynamics---------------------------------------------
	private Float azimuth;
	private Float pitch;
	private Float roll;
	
	/**
	 * @param azimuth
	 * @param pitch
	 * @param roll
	 */
	public Orientation(Float azimuth, Float pitch, Float roll)
	{
		this.azimuth = azimuth;
		this.pitch = pitch;
		this.roll = roll;
	}

	/**
	 * Copy constructor
	 * 
	 * @param another
	 */
	public Orientation(Orientation another)
	{
		this.azimuth = another.azimuth;
		this.pitch = another.pitch;
		this.roll = another.roll;
	}
	
	/**
	 * Rotation around the Z axis: 0° to 360°.
	 * 0° means the top of the device is pointing to magnetic North
	 * 
	 * @return the azimuth
	 */
	public float getAzimuth()
	{
		if(azimuth == null)
			return 0.0f;
		return azimuth;
	}

	public boolean hasAzimuth()
	{
		return azimuth != null;
	}
	
	/**
	 * Rotation around the X axis: -90° to 90°.
	 * 90° mean the device is pointed to the ground, -90° means it is pointed to the sky.
	 * 
	 * @return the pitch
	 */
	public float getPitch()
	{
		if(pitch == null)
			return 0.0f;
		return pitch;
	}

	public boolean hasPitch()
	{
		return pitch != null;
	}
	
	/**
	 * Rotation around the Y axis: -180° to 180°.
	 * 0° means the device is lying on its back (screen facing upwards), (-)180° means it is lying on its "face" (screen facing downwards).
	 * 
	 * @return the roll
	 */
	public float getRoll()
	{
		if(roll == null)
			return 0.0f;
		return roll;
	}
	
	public boolean hasRoll()
	{
		return roll != null;
	}

	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (azimuth == null ? 0 : azimuth.hashCode());
		hash = 31 * hash + (pitch == null ? 0 : pitch.hashCode());
		hash = 31 * hash + (roll == null ? 0 : roll.hashCode());
		return hash;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof Orientation)
		{
			Orientation other = (Orientation) obj;
			return	(this.azimuth == null ? other.azimuth == null : this.azimuth.equals(other.azimuth)) &&
					(this.pitch == null ? other.pitch == null : this.pitch.equals(other.pitch)) &&
					(this.roll == null ? other.roll == null : this.roll.equals(other.roll));
		}
		else
			return false;
	}
	
	public String toString()
	{
		return	(azimuth != null ? azimuth.toString() : "") + SEPARATOR +
				(pitch != null ? pitch.toString() : "") + SEPARATOR +
				(roll != null ? roll.toString() : "");
	}
	
	static public String GetStringHeader(String prefix, String separator)
	{
		return 	prefix + "azimuth" + separator +
				prefix + "pitch" + separator +
				prefix + "roll";
	}
	
}
