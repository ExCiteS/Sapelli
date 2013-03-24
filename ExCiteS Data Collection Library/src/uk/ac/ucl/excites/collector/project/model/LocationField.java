/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.project.util.Timeoutable;
import uk.ac.ucl.excites.storage.model.LocationColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.types.Location;

/**
 * @author mstevens
 *
 */
public class LocationField extends Field implements Timeoutable
{
	
	//Statics----------------------------------------------
	public static final int TYPE_ANY = 0;
	public static final int TYPE_GPS = 1;
	public static final int TYPE_NETWORK = 2;
	//public static final int TYPE_PASSIVE = 3;
	
	//Defaults:
	public static final int DEFAULT_TYPE = TYPE_GPS; 				//use GPS
	public static final boolean DEFAULT_START_WITH_FORM = true;		//start listening for locations at the start of the form
	public static final boolean DEFAULT_WAIT_AT_FIELD = false;		//do not wait for a new(er) location when at the field
	public static final int DEFAULT_TIMEOUT_S = 5 * 60; 			//use timeout of 5 minutes (300 seconds)
	public static final int DEFAULT_MAX_AGE_S = 10 * 60;			//use max age of 10 minutes (600 seconds)
	public static final float DEFAULT_MAX_ACCURACY_RADIUS = 75;		//use a maximum acceptably accuracy radius of 75m (small is better) 
	public static final boolean DEFAULT_USE_BEST_NON_QUALIFYING_LOCATION_AFTER_TIMEOUT = true;		//take the best known location (even though it does not meet type/age/accuracy requirements) if timeout passes
	public static final boolean DEFAULT_DOUBLE_PRECISION = false; 	//use 32 bit floats for lat, lon & alt
	public static final boolean DEFAULT_STORE_ALTITUDE = true;		//store altitude
	public static final boolean DEFAULT_STORE_BEARING = false;		//do not store bearing
	public static final boolean DEFAULT_STORE_SPEED = false;		//do not store speed
	public static final boolean DEFAULT_STORE_ACCURACY = true;		//store accuracy
	public static final boolean DEFAULT_STORE_PROVIDER = false;		//do not store provider
	
	
	//Dynamics---------------------------------------------
	private int type;
	private boolean startWithForm;
	private boolean waitAtField;
	private int timeoutS;
	private int maxAgeS;
	private float maxAccuracyRadius;
	private boolean useBestNonQualifyingLocationAfterTimeout;
	private boolean doublePrecision;
	private boolean storeAltitude;
	private boolean storeBearing;
	private boolean storeSpeed;
	private boolean storeAccuracy;
	private boolean storeProvider;
	
	public LocationField(Form form, String id)
	{
		super(form, id);
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
	 * @return the maxAgeS
	 */
	public int getMaxAgeS()
	{
		return maxAgeS;
	}

	/**
	 * @param maxAgeS the maxAgeS to set
	 */
	public void setMaxAgeS(int maxAgeS)
	{
		this.maxAgeS = maxAgeS;
	}

	/**
	 * @return the maxAccuracyRadius
	 */
	public float getMaxAccuracyRadius()
	{
		return maxAccuracyRadius;
	}

	/**
	 * @param maxAccuracyRadius the maxAccuracyRadius to set
	 */
	public void setMaxAccuracyRadius(float maxAccuracyRadius)
	{
		this.maxAccuracyRadius = maxAccuracyRadius;
	}

	/**
	 * @return the useBestNonQualifyingLocationAfterTimeout
	 */
	public boolean isUseBestNonQualifyingLocationAfterTimeout()
	{
		return useBestNonQualifyingLocationAfterTimeout;
	}

	/**
	 * @param useBestNonQualifyingLocationAfterTimeout the useBestNonQualifyingLocationAfterTimeout to set
	 */
	public void setUseBestNonQualifyingLocationAfterTimeout(boolean useBestNonQualifyingLocationAfterTimeout)
	{
		this.useBestNonQualifyingLocationAfterTimeout = useBestNonQualifyingLocationAfterTimeout;
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
		return new LocationColumn(id, (optional != Optionalness.NEVER), doublePrecision, storeAltitude, storeBearing, storeSpeed, storeAccuracy, storeProvider);
	}
	
	public boolean storeLocation(Record record, Location location)
	{
		return storeLocation(record, location, false);
	}
	
	public boolean storeLocation(Record record, Location location, boolean bestWeCouldGet)
	{	
		//Null check:
		if(location == null)
			return false;
		if(!bestWeCouldGet)
		{
			//Time check:
			long ageMS = System.currentTimeMillis() - location.getTime();
			if(ageMS > maxAgeS * 1000)
				return false; //location is too old
			//Provider type check:
			if(type == LocationField.TYPE_GPS && location.getProvider() != Location.PROVIDER_GPS)
				return false;
			//Accuracy check:
			if(location.getAccuracy() > maxAccuracyRadius)
				return false; //not accurate enough
		}
		//this location is good enough or it is the best we could get, so store it:
		((LocationColumn) column).storeValue(record, location);
		return true;
	}
	
	public Location retrieveLocation(Record entry)
	{
		return ((LocationColumn) column).retrieveValue(entry);
	}
		
	@Override
	public void setIn(CollectorUI ui)
	{
		ui.setLocation(this);
	}
	
}
