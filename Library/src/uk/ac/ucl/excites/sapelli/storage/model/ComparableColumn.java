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

/**
 * Column with support for comparing values of generic type {@code T}.
 * 
 * @param <T>
 * @author mstevens
 */
public abstract class ComparableColumn<T> extends Column<T> implements Comparator<ValueSet<?>>
{
	
	static private final long serialVersionUID = 2L;

	public ComparableColumn(String name, boolean optional)
	{
		super(name, optional);
	}
	
	/* (non-Javadoc)
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	@Override
	public int compare(ValueSet<?> lhs, ValueSet<?> rhs)
	{
		return lhs == null ?
				(rhs == null ? 0 : Integer.MIN_VALUE) :
				(rhs == null ? Integer.MAX_VALUE : compareValues(retrieveValue(lhs), retrieveValue(rhs)));
	}
	
	/**
	 * Alias for {@link #compare(ValueSet<?>, ValueSet<?>)}
	 * 
	 * @param vs1
	 * @param vs2
	 * @return comparison result
	 */
	public int retrieveAndCompareValues(ValueSet<?> vs1, ValueSet<?> vs2)
	{
		return compare(vs1, vs2);
	}
	
	/**
	 * @param vs (probably shouldn't be null, but if it we will compare the given value to null)
	 * @param value (may be null if column is optional)
	 * @return comparison result
	 */
	public int retrieveAndCompareToValue(ValueSet<?> vs, T value)
	{
		return compareValues(vs != null ? retrieveValue(vs) : null, value);
	}
	
	/**
	 * @param vs (probably shouldn't be null, but if it we will compare the given value to null)
	 * @param value (as object, may be null if column is optional)
	 * @return comparison result
	 * @throws IllegalArgumentException in case of a schema mismatch or invalid value
	 * @throws NullPointerException if value is null on an non-optional column
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type <T>
	 */
	@SuppressWarnings("unchecked")
	public int retrieveAndCompareToObject(ValueSet<?> vs, Object value) throws ClassCastException
	{
		return compareValues(vs != null ? retrieveValue(vs) : null, (T) convert(value));
	}
	
	/**
	 * @param lhs left-hand side value, possibly null
	 * @param rhs right-hand side value, possibly null 
	 * @return comparison result
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 * @see <a href="http://stackoverflow.com/a/128220/1084488">http://stackoverflow.com/a/128220/1084488</a>
	 */
	public int compareValues(T lhs, T rhs)
	{
		return lhs == null ?
				(rhs == null ? 0 : Integer.MIN_VALUE) :
				(rhs == null ? Integer.MAX_VALUE : compareNonNullValues(lhs, rhs));
	}
	
	/**
	 * To be implemented by subclasses, arguments are guaranteed to both be non-null.
	 * 
	 * @param lhs left-hand side value, guaranteed non-null
	 * @param rhs right-hand side value, guaranteed non-null
	 * @return comparison result
	 */
	protected abstract int compareNonNullValues(T lhs, T rhs);
	
	public Comparator<T> getValueComparator()
	{
		return new Comparator<T>()
		{
			@Override
			public int compare(T lhs, T rhs)
			{
				return compareValues(lhs, rhs);
			}
		};
	}

}
