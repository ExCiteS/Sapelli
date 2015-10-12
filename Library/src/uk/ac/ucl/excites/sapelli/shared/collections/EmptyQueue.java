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

package uk.ac.ucl.excites.sapelli.shared.collections;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Queue;

import uk.ac.ucl.excites.sapelli.shared.util.Collections7;

/**
 * @author mstevens
 *
 * @param <E>
 */
public final class EmptyQueue<E> implements Queue<E>
{

	@Override
	public int size()
	{
		return 0;
	}

	@Override
	public boolean isEmpty()
	{
		return true;
	}

	@Override
	public boolean contains(Object o)
	{
		return false;
	}

	@Override
	public Iterator<E> iterator()
	{
		/* Note:
		 * 	We use our Collections7 instead of java.util.Collection to make sure this class works on
		 * 	Java/JRE version prior to [1.]7 and Android versions prior to 4.4/API level 19. */
		return Collections7.<E> emptyIterator();
	}

	@Override
	public Object[] toArray()
	{
		return new Object[0];
	}

	@Override
	public <T> T[] toArray(T[] a)
	{
		if(a.length > 0)
            a[0] = null;
        return a;
	}

	@Override
	public boolean remove(Object o)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean containsAll(Collection<?> c)
	{
		return false;
	}

	@Override
	public boolean addAll(Collection<? extends E> c)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean removeAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean retainAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public void clear()
	{
	}

	@Override
	public boolean add(E e)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean offer(E e)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public E remove()
	{
		throw new NoSuchElementException();
	}

	@Override
	public E poll()
	{
		return null;
	}

	@Override
	public E element()
	{
		throw new NoSuchElementException();
	}

	@Override
	public E peek()
	{
		return null;
	}

}
