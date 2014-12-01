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

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.LocationUI;
import uk.ac.ucl.excites.sapelli.shared.util.Timeoutable;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Location;

/**
 * @author mstevens
 *
 */
public class LocationField extends Field implements Timeoutable
{
	
	//Statics----------------------------------------------
	static public final int TYPE_ANY = 0;
	static public final int TYPE_GPS = 1;
	static public final int TYPE_NETWORK = 2;
	//static public final int TYPE_PASSIVE = 3;
	static public enum START_WITH {
		FORM,
		PAGE,
		FIELD
	};
	
	//Defaults:
	static public final int DEFAULT_TYPE = TYPE_GPS; 				// use GPS by default
	static public final START_WITH DEFAULT_START_WITH = START_WITH.FORM;	// start listening for locations at the start of the form
	static public final boolean DEFAULT_WAIT_AT_FIELD = false;		// do not wait for a new(er) location when at the field
	static public final int DEFAULT_TIMEOUT_S = 5 * 60; 			// use timeout of 5 minutes (300 seconds)
	static public final int DEFAULT_MAX_AGE_S = 10 * 60;			// use max age of 10 minutes (600 seconds)
	static public final float DEFAULT_MAX_ACCURACY_RADIUS = 75;		// use a maximum acceptably accuracy radius of 75m (small is better) 
	static public final boolean DEFAULT_USE_BEST_NON_QUALIFYING_LOCATION_AFTER_TIMEOUT = true;		// take the best known location (even though it does not meet type/age/accuracy requirements) if timeout passes
	static public final boolean DEFAULT_DOUBLE_PRECISION = false; 	// use 32 bit floats for lat, lon & alt
	static public final boolean DEFAULT_STORE_ALTITUDE = true;		// store altitude
	static public final boolean DEFAULT_STORE_BEARING = false;		// do not store bearing
	static public final boolean DEFAULT_STORE_SPEED = false;		// do not store speed
	static public final boolean DEFAULT_STORE_ACCURACY = true;		// store accuracy
	static public final boolean DEFAULT_STORE_PROVIDER = false;		// do not store provider
	
	//Dynamics---------------------------------------------
	private int type;
	private START_WITH startWith;
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
	
	/**
	 * @param form the form the field belongs to
	 * @param id the id of the field, should not be null
	 * @param caption the caption of the field, may be null (in which case the id is used as the caption)
	 */
	public LocationField(Form form, String id, String caption)
	{
		super(form, id, caption);
		if(id == null)
			throw new NullPointerException("ID of top-level field cannot be null");
		this.type = DEFAULT_TYPE;
		this.startWith = DEFAULT_START_WITH;
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
	 * @return the startWith
	 */
	public START_WITH getStartWith()
	{
		return startWith;
	}

	/**
	 * @param startWithForm the startWith to set
	 */
	public void setStartWith(START_WITH startWith)
	{
		this.startWith = startWith;
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
	protected LocationColumn createColumn(String name)
	{
		return new LocationColumn(	name,
									form.getColumnOptionalityAdvisor().getColumnOptionality(this),
									doublePrecision,
									storeAltitude,
									storeBearing,
									storeSpeed,
									storeAccuracy,
									false, // for now time is never stored (or rather transmited)
									storeProvider);
	}
	
	@Override
	public LocationColumn getColumn()
	{
		return (LocationColumn) super.getColumn();
	}
	
	public boolean storeLocation(Record record, Location location)
	{
		return storeLocation(record, location, false);
	}
	
	public boolean storeLocation(Record record, Location location, boolean bestWeCouldGet)
	{	
		if(isNoColumn())
			return true; // just in case
		if(isAcceptable(location, bestWeCouldGet))
		{
			// This location is good enough or it is the best we could get, so store it:
			getColumn().storeValue(record, location);
			return true;
		}
		return false;
	}
	
	public boolean isAcceptable(Location location, boolean bestWeCouldGet)
	{
		// Null check:
		if(location == null)
			return false;
		if(!bestWeCouldGet)
		{
			// Time check:
			long ageMS = System.currentTimeMillis() - location.getTime().getMsSinceEpoch();
			if(ageMS > maxAgeS * 1000)
				return false; //location is too old
			// Provider type check:
			if(type == LocationField.TYPE_GPS && location.getProvider() != Location.PROVIDER_GPS)
				return false;
			// Accuracy check:
			if(location.getAccuracy() > maxAccuracyRadius)
				return false; //not accurate enough
		}
		return true;
	}
	
	public Location retrieveLocation(Record record)
	{
		return ((LocationColumn) form.getColumnFor(this)).retrieveValue(record);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterLocationField(this, arguments, withPage);
	}
	
	@Override
	public <V, UI extends CollectorUI<V, UI>> LocationUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createLocationUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof LocationField)
		{
			LocationField that = (LocationField) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.type == that.type &&
					this.startWith == that.startWith &&
					this.timeoutS == that.timeoutS &&
					this.maxAgeS == that.maxAgeS &&
					this.maxAccuracyRadius == that.maxAccuracyRadius &&
					this.useBestNonQualifyingLocationAfterTimeout == that.useBestNonQualifyingLocationAfterTimeout &&
					this.doublePrecision == that.doublePrecision &&
					this.storeAltitude == that.storeAltitude &&
					this.storeBearing == that.storeBearing &&
					this.storeSpeed == that.storeSpeed &&
					this.storeAccuracy == that.storeAccuracy &&
					this.storeProvider == that.storeProvider;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + type;
		hash = 31 * hash + (startWith.ordinal());
		hash = 31 * hash + (waitAtField ? 0 : 1);
		hash = 31 * hash + timeoutS;
		hash = 31 * hash + maxAgeS;
		hash = 31 * hash + Float.floatToIntBits(maxAccuracyRadius);
		hash = 31 * hash + (useBestNonQualifyingLocationAfterTimeout ? 0 : 1);
		hash = 31 * hash + (doublePrecision ? 0 : 1);
		hash = 31 * hash + (storeAltitude ? 0 : 1);
		hash = 31 * hash + (storeBearing ? 0 : 1);
		hash = 31 * hash + (storeSpeed ? 0 : 1);
		hash = 31 * hash + (storeAccuracy ? 0 : 1);
		hash = 31 * hash + (storeProvider ? 0 : 1);
		return hash;
	}
	
}
