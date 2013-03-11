/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.FieldView;
import uk.ac.ucl.excites.storage.model.LocationColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.types.Location;

/**
 * @author mstevens
 *
 */
public class LocationField extends Field
{
	
	//Statics----------------------------------------------
	public static final int TYPE_ANY = 0;
	public static final int TYPE_GPS = 1;
	public static final int TYPE_NETWORK = 2;
	//public static final int TYPE_PASSIVE = 3;
	
	public static final int LISTENER_UPDATE_MIN_TIME_MS = 15 * 1000;//30 seconds 
	public static final int LISTENER_UPDATE_MIN_DISTANCE_M = 5; 	//5 meters
	
	//Defaults:
	public static final int DEFAULT_TYPE = TYPE_GPS; 				//use GPS by default
	public static final boolean DEFAULT_START_WITH_FORM = true;		//start listingen for location at the start of the form by default
	public static final boolean DEFAULT_WAIT_AT_FIELD = false;		//do not wait for a new(er) location when at the field by default
	public static final int DEFAULT_TIMEOUT_S = 300; 				//use timeout of 300 seconds (5 minutes) by default
	public static final boolean DEFAULT_DOUBLE_PRECISION = false; 	//use 32 bit floats for lat, lon & alt by default
	public static final boolean DEFAULT_STORE_ALTITUDE = true;		//store altitude by default
	public static final boolean DEFAULT_STORE_BEARING = false;		//do not store bearing by default
	public static final boolean DEFAULT_STORE_SPEED = false;		//do not store speed by default
	public static final boolean DEFAULT_STORE_ACCURACY = true;		//store accuracy by default
	public static final boolean DEFAULT_STORE_PROVIDER = false;		//do not store provider by default
	
	
	//Dynamics---------------------------------------------
	private int type;
	private boolean startWithForm;
	private boolean waitAtField;
	private int timeoutS;
	private boolean doublePrecision;
	private boolean storeAltitude;
	private boolean storeBearing;
	private boolean storeSpeed;
	private boolean storeAccuracy;
	private boolean storeProvider;
	
	public LocationField(String id)
	{
		super(id);
		if(id == null)
			throw new NullPointerException("ID of top-level field cannot be null");
		this.type = DEFAULT_TYPE;
		this.startWithForm = DEFAULT_START_WITH_FORM;
		this.waitAtField = DEFAULT_WAIT_AT_FIELD;
		this.timeoutS = DEFAULT_TIMEOUT_S;
		this.doublePrecision = DEFAULT_DOUBLE_PRECISION;
		this.storeAltitude = DEFAULT_STORE_ALTITUDE;
		this.storeBearing = DEFAULT_STORE_BEARING;
		this.storeSpeed = DEFAULT_STORE_SPEED;
		this.storeAccuracy = DEFAULT_STORE_ACCURACY;
		this.storeProvider = DEFAULT_STORE_PROVIDER;
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
		if(type < TYPE_ANY || type > TYPE_NETWORK)
			throw new IllegalArgumentException("Unknown location provider type (" + type + "!");
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
	 * @return whether or not lat, lon & alt will be stored as 64 bit doubles (true) or 32 bit floats (false)
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
	 * @return the startWithForm
	 */
	public boolean isStartWithForm()
	{
		return startWithForm;
	}

	/**
	 * @param startWithForm the startWithForm to set
	 */
	public void setStartWithForm(boolean startWithForm)
	{
		this.startWithForm = startWithForm;
	}

	/**
	 * @return the waitAtField
	 */
	public boolean isWaitAtField()
	{
		return waitAtField;
	}

	/**
	 * @param waitAtField the waitAtField to set
	 */
	public void setWaitAtField(boolean waitAtField)
	{
		this.waitAtField = waitAtField;
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

	/**
	 * @return the storeProvider
	 */
	public boolean isStoreProvider()
	{
		return storeProvider;
	}

	/**
	 * @param storeProvider the storeProvider to set
	 */
	public void setStoreProvider(boolean storeProvider)
	{
		this.storeProvider = storeProvider;
	}

	@Override
	protected LocationColumn createColumn()
	{
		return new LocationColumn(id, true /*always optional because we can never be sure to get coordinates*/, doublePrecision, storeAltitude, storeBearing, storeSpeed, storeAccuracy, storeProvider);
	}
	
	public boolean storeLocation(Location location, Record entry)
	{	
		//Time check
		//TODO
		//Provider type check
		if(type == LocationField.TYPE_GPS && location.getProvider() != Location.PROVIDER_GPS)
			return false;
		//Accuracy check
		//TODO
		
		//Ok location is good enough, so store it:
		((LocationColumn) column).storeValue(entry, location);
		return true;
	}
	
	public Location retrieveLocation(Record entry)
	{
		return ((LocationColumn) column).retrieveValue(entry);
	}
		
	@Override
	public void setIn(FieldView fv)
	{
		fv.setLocation(this);
	}
	
}
