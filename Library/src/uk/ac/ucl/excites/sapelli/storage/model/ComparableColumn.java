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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.util.Comparator;

import uk.ac.ucl.excites.sapelli.storage.util.InvalidValueException;

/**
 * Column with support for comparing (for grouping AND ranking) values of generic type {@code T}.
 * 
 * @param <T>
 * @author mstevens
 */
public abstract class ComparableColumn<T> extends Column<T> implements Comparator<ValueSet<?>>
{
	
	static private final long serialVersionUID = 2L;

	/**
	 * @param name
	 * @param optional
	 * @param defaultValue
	 * @throws IllegalArgumentException in case the given name is invalid
	 * @throws InvalidValueException in case the given defaultValue is invalid
	 */
	public ComparableColumn(String name, boolean optional, T defaultValue) throws IllegalArgumentException, InvalidValueException
	{
		super(name, optional, defaultValue);
	}
	
	/**
	 * To be implemented by subclasses, arguments are guaranteed to both be non-null.
	 * 
	 * @param lhs left-hand side value, guaranteed non-null
	 * @param rhs right-hand side value, guaranteed non-null
	 * @return comparison result
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#compareNonNullValues(java.lang.Object, java.lang.Object)
	 */
	protected abstract int compareNonNullValues(T lhs, T rhs);

}
