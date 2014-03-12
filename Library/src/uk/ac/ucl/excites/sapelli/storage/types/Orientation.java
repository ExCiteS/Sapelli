package uk.ac.ucl.excites.sapelli.storage.types;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;

/**
 * @author mstevens
 *
 */
public class Orientation extends Record
{
	
	//Statics----------------------------------------------
	// Schema & columns
	static final public Schema SCHEMA = new Schema(Schema.ReservedUsageIDs.ORIENTATION_SCHEMA.ordinal(), Schema.ReservedUsageIDs.ORIENTATION_SCHEMA.name());
	static final public FloatColumn COLUMN_AZIMUTH = new FloatColumn("Azimuth", true);
	static final public FloatColumn COLUMN_PITCH = new FloatColumn("Pitch", true);
	static final public FloatColumn COLUMN_ROLL = new FloatColumn("Roll", true);
	static
	{
		SCHEMA.addColumn(COLUMN_AZIMUTH);
		SCHEMA.addColumn(COLUMN_PITCH);
		SCHEMA.addColumn(COLUMN_ROLL);
		SCHEMA.seal();
	}
	
	/**
	 * @param azimuth
	 * @param pitch
	 * @param roll
	 */
	public Orientation(Float azimuth, Float pitch, Float roll)
	{
		super(SCHEMA);
		setValue(COLUMN_AZIMUTH, azimuth);
		setValue(COLUMN_PITCH, pitch);
		setValue(COLUMN_ROLL, roll);
	}
	
	public Orientation()
	{
		super(SCHEMA);
	}

	/**
	 * Copy constructor
	 * 
	 * @param another
	 */
	public Orientation(Orientation another)
	{
		super(another);
	}
	
	private float getPart(FloatColumn column)
	{
		Float value = (Float) getValue(column);
		if(value == null)
			return 0.0f;
		return value;
	}
	
	/**
	 * Rotation around the Z axis: 0 to 360 degrees.
	 * 0 degrees means the top of the device is pointing to magnetic North
	 * 
	 * @return the azimuth
	 */
	public float getAzimuth()
	{
		return getPart(COLUMN_AZIMUTH);
	}

	public boolean hasAzimuth()
	{
		return getValue(COLUMN_AZIMUTH) != null;
	}
	
	/**
	 * Rotation around the X axis: -90 to 90 degrees.
	 * 90 degrees means the device is pointed to the ground, -90 degrees means it is pointed to the sky.
	 * 
	 * @return the pitch
	 */
	public float getPitch()
	{
		return getPart(COLUMN_PITCH);
	}

	public boolean hasPitch()
	{
		return getValue(COLUMN_PITCH) != null;
	}
	
	/**
	 * Rotation around the Y axis: -180 to 180 degrees.
	 * 0 degrees means the device is lying on its back (screen facing upwards), (-)180 degrees means it is lying on its "face" (screen facing downwards).
	 * 
	 * @return the roll
	 */
	public float getRoll()
	{
		return getPart(COLUMN_ROLL);
	}
	
	public boolean hasRoll()
	{
		return getValue(COLUMN_ROLL) != null;
	}
	
}
