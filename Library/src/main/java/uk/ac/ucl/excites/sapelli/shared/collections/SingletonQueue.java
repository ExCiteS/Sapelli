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

package uk.ac.ucl.excites.sapelli.shared.collections;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Queue;

/**
 * @author mstevens
 *
 */
public final class SingletonQueue<E> implements Queue<E>
{

	private final E e;
	private boolean empty = false; 
	
	public SingletonQueue(final E e)
	{
		this.e = e;
	}
	
	@Override
	public int size()
	{
		return empty ? 0 : 1;
	}

	@Override
	public boolean isEmpty()
	{
		return empty;
	}

	@Override
	public boolean contains(Object o)
	{
		return !empty && (o == null ? e == null : o.equals(e));
	}

	@Override
	public Iterator<E> iterator()
	{
		return new Iterator<E>()
		{

			private boolean atEnd = false;
			
			@Override
			public boolean hasNext()
			{
				return !atEnd;
			}

			@Override
			public E next()
			{
				if(atEnd)
					throw new NoSuchElementException();
				atEnd = true;
				return e;
			}
		};
	}

	@Override
	public Object[] toArray()
	{
		return empty ? new Object[0] : new Object[] { e };
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T[] toArray(T[] a)
	{
		if(!empty)
			a[0] = (T) e;
		return a;
	}

	@Override
	public boolean remove(Object o)
	{
		return contains(o) ? empty = true : false;
	}

	@Override
	public boolean containsAll(Collection<?> c)
	{
		for(Object ce : c)
			if(!contains(ce))
				return false;
		return true;
	}

	@Override
	public boolean addAll(Collection<? extends E> c)
	{
		throw new UnsupportedOperationException("Cannot add to SingletonQueue");
	}

	@Override
	public boolean removeAll(Collection<?> c)
	{
		if(empty)
			return false;
		else
		{
			for(Object ce : c)
				remove(ce);
			return empty;
		}
	}

	@Override
	public boolean retainAll(Collection<?> c)
	{
		if(!empty && !c.contains(e))
			return empty = true;
		return false;
	}

	@Override
	public void clear()
	{
		empty = true;
	}

	@Override
	public boolean add(E e)
	{
		throw new UnsupportedOperationException("Cannot add to SingletonQueue");
	}

	@Override
	public boolean offer(E e)
	{
		throw new UnsupportedOperationException("Cannot add to SingletonQueue");
	}

	@Override
	public E remove()
	{
		if(empty)
			throw new NoSuchElementException();
		empty = true;
		return e;
	}

	@Override
	public E poll()
	{
		if(empty)
			return null;
		empty = true;
		return e;
	}

	@Override
	public E element()
	{
		if(empty)
			throw new NoSuchElementException();
		return e;
	}

	@Override
	public E peek()
	{
		if(empty)
			return null;
		return e;
	}

}
