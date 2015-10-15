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
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;

/**
 * A class representing 3-dimensional (device) orientation.
 * Implemented as a Record subclass. 
 * 
 * @author mstevens
 */
public class Orientation extends ValueSet<ColumnSet>
{
	
	//Statics----------------------------------------------
	static private final long serialVersionUID = 2L;
	
	// ColumnSet & Columns:
	static final public ColumnSet COLUMN_SET = new ColumnSet(Orientation.class.getSimpleName(), false);
	static final public FloatColumn COLUMN_AZIMUTH	= COLUMN_SET.addColumn(new FloatColumn("Azimuth", true, true, false));				// optional signed 32 bit float
	static final public FloatColumn COLUMN_PITCH	= COLUMN_SET.addColumn(new FloatColumn("Pitch", true, true, false));				// optional signed 32 bit float
	static final public FloatColumn COLUMN_ROLL		= COLUMN_SET.addColumn(new FloatColumn("Roll", true, true, false), true /*seal!*/);	// optional signed 32 bit float
	
	/**
	 * @param azimuth
	 * @param pitch
	 * @param roll
	 */
	public Orientation(Float azimuth, Float pitch, Float roll)
	{
		super(COLUMN_SET);
		COLUMN_AZIMUTH.storeValue(this, azimuth);
		COLUMN_PITCH.storeValue(this, pitch);
		COLUMN_ROLL.storeValue(this, roll);
	}
	
	/**
	 * Only to be used by {@link OrientationColumn#getNewRecord()}
	 */
	/*package*/ Orientation()
	{
		super(COLUMN_SET); // no default values!
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
