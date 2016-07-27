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

package uk.ac.ucl.excites.sapelli.shared.util;

import java.util.Collection;
import java.util.Stack;

/**
 * @author mstevens
 * 
 */
public final class CollectionUtils
{

	private CollectionUtils() {}

	/**
	 * Adds an element to the collection unless the element is null.
	 * Taken from Apache Commons Collections library (licensed under Apache License v2.0)
	 * 
	 * @param <T>  the type of object the {@link Collection} contains
	 * @param collection  the collection to add to, must not be null
	 * @param object  the object to add, if null it will not be added
	 * @return true if the collection changed
	 * @throws NullPointerException if the collection is null
	 * @see <a href="http://svn.apache.org/viewvc/commons/proper/collections/trunk/src/main/java/org/apache/commons/collections4/CollectionUtils.java?view=co">Apache Commons Collections (CollectionUtils.java)</a>
	 */
	public static <T> boolean addIgnoreNull(final Collection<T> collection, final T object)
	{
		if(collection == null)
			throw new NullPointerException("The collection must not be null");
		return object != null && collection.add(object);
	}
	
	/**
	 * Add elements from a source collection to a target collection unless they or the source collection itself are null.
	 * 
	 * @param target  target collection
	 * @param source  source collection
	 * @return true if the target collection changed
	 * @throws NullPointerException if the target collection is null
	 */
	public static <T> boolean addAllIgnoreNull(final Collection<T> target, final Collection<T> source)
	{
		if(target == null)
			throw new NullPointerException("The target collection must not be null");
		if(source == null)
			return false;
		boolean changed = false;
		for(T element : source)
			changed |= addIgnoreNull(target, element);
		return changed;
	}
	
	/**
	 * Returns a pretty-printed version of the collection. 
	 * Items will be toString()'ed and separated by commas.
	 * The collection can be optionally delimited by "[]".
	 * 
	 * @param collection
	 * @param dilimit
	 * @return
	 */
	public static String allToString(final Collection<?> collection, boolean delimit)
	{
		boolean first = true;
		StringBuilder bldr = new StringBuilder(delimit ? "[" : "");
		for(Object o : collection)
		{
			bldr.append((first ? "" : ", ") + o.toString());
			first = false;
		}
		if(delimit)
			bldr.append(']');
		return bldr.toString();
	}
	
	/**
	 * Combinations of {@link Stack#pop()} and {@link Stack#push(Object)}.
	 * If the given stack is non-empty the current top item is removed (i.e. {@code pop}ped),
	 * and then the given item is {@code push}ed on the stack, thus replacing the previous
	 * top item if there was one. 
	 * 
	 * @param stack
	 * @param item
	 * @return
	 */
	public static <T> T poke(final Stack<T> stack, final T item)
	{
		if(!stack.isEmpty())
			stack.pop(); // remove current top
		return stack.push(item);
	}
	
}
