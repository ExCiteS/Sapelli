/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.collector.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author Michalis Vitos, mstevens
 *
 */
public class Trigger extends JumpSource
{

	// Statics----------------------------------------------
	static public final int NO_TIMEOUT = -1;
	static public String KEY_SEPARATOR = "\\|";
	
	static public enum Key
	{
		ANY,
		BACK,
		SEARCH,
		HOME,
		VOLUME_DOWN,
		VOLUME_MUTE,
		VOLUME_UP,
		// more later?
	}
	
	// Dynamics---------------------------------------------
	protected List<Key> keys;
	protected int fixedTimer = NO_TIMEOUT;

	/**
	 * @return the keys
	 */
	public List<Key> getKeys()
	{
		return keys != null ? keys : Collections.<Key> emptyList();
	}
	
	public void addKey(Key key)
	{
		if(this.keys == null)
			this.keys = new ArrayList<Trigger.Key>();
		if(key == null)
			throw new NullPointerException("Key cannot be null!");
		this.keys.add(key);
	}

	/**
	 * @return the fixedTimer
	 */
	public int getFixedTimer()
	{
		return fixedTimer;
	}

	/**
	 * @param fixedTimer the fixedTimer to set
	 */
	public void setFixedTimer(int fixedTimer)
	{
		if(fixedTimer != NO_TIMEOUT && fixedTimer < 0)
			throw new IllegalArgumentException("Invalid timer duration: " + fixedTimer);
		this.fixedTimer = fixedTimer;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof Trigger)
		{
			Trigger that = (Trigger) obj;
			return	super.equals(that) && // JumpSource#equals(Object)
					(this.keys != null ? this.keys.equals(that.keys) : that.keys == null) &&
					this.fixedTimer == that.fixedTimer;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // JumpSource#hashCode()
		if(keys == null)
			hash = 31 * hash + 0;
		else
			for(Key k : keys) // We do this instead of keys.hashCode() because we're not sure whether all Java implementations use ordinal() as the hashCode() of an Enum.
				hash = 31 * hash + k.ordinal();
		hash = 31 * hash + fixedTimer;
		return hash;
	}
	
}
