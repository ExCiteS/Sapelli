package uk.ac.ucl.excites.util;

import java.util.Collection;

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
	
}
