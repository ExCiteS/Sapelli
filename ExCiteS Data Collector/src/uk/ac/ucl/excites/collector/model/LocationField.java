/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

import uk.ac.ucl.excites.storage.model.LocationColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class LocationField extends Field
{
	
	//Statics----------------------------------------------
	public static final int TYPE_GPS = 1;
	public static final int TYPE_NETWORK = 2;
	
	//Defaults:
	public static final int DEFAULT_TYPE = TYPE_GPS; 				//use GPS by default
	public static final int DEFAULT_TIMEOUT_S = 300; 				//use timeout of 300 seconds (5 minutes) by default
	public static final boolean DEFAULT_DOUBLE_PRECISION = false; 	//use 32 bit floats for lat, lon & alt by default
	public static final boolean DEFAULT_STORE_ALTITUDE = true;		//store altitude by default
	public static final boolean DEFAULT_STORE_BEARING = false;		//do not store bearing by default
	public static final boolean DEFAULT_STORE_SPEED = false;		//do not store speed by default
	public static final boolean DEFAULT_STORE_ACCURACY = true;		//store accuracy by default
	
	
	//Dynamics---------------------------------------------
	private int type;
	private int timeoutS;
	private boolean doublePrecision;
	private boolean storeAltitude;
	private boolean storeBearing;
	private boolean storeSpeed;
	private boolean storeAccuracy;
	
	public LocationField(String id)
	{
		super(id);
		if(id == null)
			throw new NullPointerException("ID of top-level field cannot be null");
		this.type = DEFAULT_TYPE;
		this.timeoutS = DEFAULT_TIMEOUT_S;
		this.storeAltitude = DEFAULT_STORE_ALTITUDE;
		this.storeBearing = DEFAULT_STORE_BEARING;
		this.storeSpeed = DEFAULT_STORE_SPEED;
		this.storeAccuracy = DEFAULT_STORE_ACCURACY;
	}
	
	/**
	 * @return the type
	 */
	public int getType()
	{
		return type;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(int type)
	{
		this.type = type;
	}
	
	/**
	 * This is the time to wait for coordinates while already on the waiting screen, time spent before is not counted
	 * 
	 * @return the timeoutS
	 */
	public int getTimeoutS()
	{
		return timeoutS;
	}

	/**
	 * @param timeoutS the timeoutS to set
	 */
	public void setTimeoutS(int timeoutS)
	{
		this.timeoutS = timeoutS;
	}
	
	/**
	 * @return whether or noty lat, lon & alt will be stored as 64 bit doubles (true) or 32 bit floats (false)
	 */
	public boolean isDoublePrecision()
	{
		return doublePrecision;
	}

	/**
	 * @param doublePrecision the doublePrecision to set
	 */
	public void setDoublePrecision(boolean doublePrecision)
	{
		this.doublePrecision = doublePrecision;
	}

	/**
	 * @return the storeAltitude
	 */
	public boolean isStoreAltitude()
	{
		return storeAltitude;
	}

	/**
	 * @param storeAltitude the storeAltitude to set
	 */
	public void setStoreAltitude(boolean storeAltitude)
	{
		this.storeAltitude = storeAltitude;
	}

	/**
	 * @return the storeBearing
	 */
	public boolean isStoreBearing()
	{
		return storeBearing;
	}

	/**
	 * @param storeBearing the storeBearing to set
	 */
	public void setStoreBearing(boolean storeBearing)
	{
		this.storeBearing = storeBearing;
	}

	/**
	 * @return the storeSpeed
	 */
	public boolean isStoreSpeed()
	{
		return storeSpeed;
	}

	/**
	 * @param storeSpeed the storeSpeed to set
	 */
	public void setStoreSpeed(boolean storeSpeed)
	{
		this.storeSpeed = storeSpeed;
	}

	/**
	 * @return the storeAccuracy
	 */
	public boolean isStoreAccuracy()
	{
		return storeAccuracy;
	}

	/**
	 * @param storeAccuracy the storeAccuracy to set
	 */
	public void setStoreAccuracy(boolean storeAccuracy)
	{
		this.storeAccuracy = storeAccuracy;
	}

	@Override
	public void addColumns(Schema schema)
	{
		schema.addColumn(new LocationColumn(id, true, doublePrecision, storeAltitude, storeBearing, storeSpeed, storeAccuracy));
	}
	
}
