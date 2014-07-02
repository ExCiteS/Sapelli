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
	protected Field jump;
	protected FieldParameters jumpArgs;

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
	
}
