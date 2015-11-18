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

package uk.ac.ucl.excites.sapelli.shared.util;

import java.util.*;

/**
 * Helper class to use {@link Enumeration}s as {@link Iterable}s.
 * 
 * @author mstevens
 *
 * @param <T>
 * @see http://www.javaspecialists.eu/archive/Issue107.html
 */
public class IterableEnumeration<T> implements Iterable<T>
{
	
	static public <T> Iterable<T> Make(Enumeration<T> en)
	{
		return new IterableEnumeration<T>(en);
	}
	
	private final Enumeration<T> en;

	public IterableEnumeration(Enumeration<T> en)
	{
		this.en = en;
	}

	/**
	 * Returns an adaptor for the Enumeration
	 * 
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<T> iterator()
	{
		return new Iterator<T>()
		{
			@Override
			public boolean hasNext()
			{
				return en.hasMoreElements();
			}

			@Override
			public T next()
			{
				return en.nextElement();
			}
		};
	}

}