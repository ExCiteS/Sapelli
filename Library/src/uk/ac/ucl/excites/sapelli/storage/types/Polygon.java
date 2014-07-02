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

import java.util.ArrayList;


/**
 * A polygon, implemented as a List of {@link Location}s
 * 
 * TODO check for duplicate points?
 * TODO close shape method?
 * 
 * @author mstevens
 */
public class Polygon extends ArrayList<Location>
{
	
	private static final long serialVersionUID = 2L;

	public static final int MIN_POINTS = 3;
	
	public Polygon()
	{
		super(MIN_POINTS);
	}
	
	public Polygon(int initialCapacity)
	{
		super(initialCapacity);
	}
	
	@Override
	public boolean add(Location point)
	{
		if(point != null)
			return super.add(point);
		else
			throw new NullPointerException("Cannot add null point");
	}
	
	public boolean isValid()
	{
		return size() >= MIN_POINTS; // TODO check shape, uniqueness of points, etc.
	}
	
}
