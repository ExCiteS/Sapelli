/**
 * 
 */
package uk.ac.ucl.excites.sapelli.shared.util;

import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * This class provides 100% equivalent and compatible implementations of the 3 new methods
 * which were added to {@link java.util.Collections} in JavaSE 7/JDK 1.7 (compared to 6/1.6),
 * and later also in Android 4.4/API level 19.
 * 
 * The implementation below is taken from the {@link java.util.Collections} implementation
 * created by the Android Open Source Project (libcore/luni; rev. f6b0c8b; see link below),
 * licensed under the Apache License, Version 2.0.
 * 
 * Note: new functionality added to {@link java.util.Collections} in JavaSE 8/JDK 1.8 is not provided here.
 *
 * @author AOSP, mstevens
 * 
 * @see java.util.Collections
 * @see https://android.googlesource.com/platform/libcore/+/f6b0c8b47c903d47cedf182e5aa78276283a2f32/luni/src/main/java/java/util/Collections.java
 */
public final class Collections7
{
	
	private Collections7() {}

	private static final Iterator<?> EMPTY_ITERATOR = new Iterator<Object>()
	{
		@Override
		public boolean hasNext()
		{
			return false;
		}

		@Override
		public Object next()
		{
			throw new NoSuchElementException();
		}

		@Override
		public void remove()
		{
			throw new IllegalStateException();
		}
	};

	private static final Enumeration<?> EMPTY_ENUMERATION = new Enumeration<Object>()
	{
		@Override
		public boolean hasMoreElements()
		{
			return false;
		}

		@Override
		public Object nextElement()
		{
			throw new NoSuchElementException();
		}
	};

	/**
	 * Returns an enumeration containing no elements.
	 * 
	 * @see java.util.Collections#emptyEnumeration()
	 */
	@SuppressWarnings("unchecked")
	public static <T> Enumeration<T> emptyEnumeration()
	{
		return (Enumeration<T>) EMPTY_ENUMERATION;
	}

	/**
	 * Returns an iterator containing no elements.
	 * 
	 * @see java.util.Collections#emptyIterator()
	 */
	@SuppressWarnings("unchecked")
	public static <T> Iterator<T> emptyIterator()
	{
		return (Iterator<T>) EMPTY_ITERATOR;
	}

	/**
	 * Returns a list iterator containing no elements.
	 * 
	 * @see java.util.Collections#emptyListIterator()
	 */
	public static <T> ListIterator<T> emptyListIterator()
	{
		return Collections.<T> emptyList().listIterator();
	}

}
