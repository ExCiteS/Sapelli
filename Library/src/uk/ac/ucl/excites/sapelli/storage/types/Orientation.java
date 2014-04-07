package uk.ac.ucl.excites.sapelli.storage.types;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;

/**
 * A class representing 3-dimensional (device) orientation.
 * Implemented as a Record subclass. 
 * 
 * @author mstevens
 */
public class Orientation extends Record
{
	
	//Statics----------------------------------------------
	static private final long serialVersionUID = 2L;
	
	// Schema & columns
	static final public Schema SCHEMA = new Schema(Schema.ReservedIDs.ORIENTATION_SCHEMA.ordinal(), Schema.ReservedIDs.ORIENTATION_SCHEMA.name());
	static final public FloatColumn COLUMN_AZIMUTH = new FloatColumn("Azimuth", true, true, false);	// optional signed 32 bit float
	static final public FloatColumn COLUMN_PITCH = new FloatColumn("Pitch", true, true, false);		// optional signed 32 bit float
	static final public FloatColumn COLUMN_ROLL = new FloatColumn("Roll", true, true, false);			// optional signed 32 bit float
	static
	{	// Add columns to Schema & seal it:
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
		COLUMN_AZIMUTH.storeValue(this, azimuth);
		COLUMN_PITCH.storeValue(this, pitch);
		COLUMN_ROLL.storeValue(this, roll);
	}
	
	/**
	 * Only to be used by {@link OrientationColumn#getNewRecord()}
	 */
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
	
	/**
	 * Rotation around the Z axis: 0 to 360 degrees.
	 * 0 degrees means the top of the device is pointing to magnetic North
	 * 
	 * @return the azimuth
	 */
	public float getAzimuth()
	{
		return COLUMN_AZIMUTH.getPrimitiveFloat(this, 0.0f);
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
		return COLUMN_PITCH.getPrimitiveFloat(this, 0.0f);
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
		return COLUMN_ROLL.getPrimitiveFloat(this, 0.0f);
	}
	
	public boolean hasRoll()
	{
		return getValue(COLUMN_ROLL) != null;
	}
	
}
