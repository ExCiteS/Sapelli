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

import java.util.Arrays;
import java.util.Comparator;

/**
 * This class contains {@code static} utility methods for operating on objects.
 * 
 * This class (and all its methods) is 100% equivalent and compatible with the
 * {@link java.util.Objects} class introduced in JavaSE 7/JDK 1.7, and later in Android 4.4/API level 19.
 * 
 * This implementation is loosely based on the {@link java.util.Objects} implementation created by
 * the Android Open Source Project (libcore/luni; rev. 4daf167; see link below), licensed under the
 * Apache License, Version 2.0.
 * 
 * @author AOSP, mstevens
 * 
 * @see java.util.Objects
 * @see https://android.googlesource.com/platform/libcore/+/4daf167bcaae54986cd4d9ad9b604700fde8f2fe/luni/src/main/java/java/util/Objects.java
 */
public final class Objects
{

	private Objects()
	{
		// should never be instantiated
	}
	
	/**
	 * Returns 0 if {@code a == b}, or {@code c.compare(a, b)} otherwise. That is, this makes {@code c} null-safe.
	 * 
	 * @see java.util.Objects#compare(Object, Object, Comparator)
	 * @see java.util.Comparator#compare(Object, Object)
	 */
	public static <T> int compare(T a, T b, Comparator<? super T> c)
	{
		return a == b ? 0 : c.compare(a, b);
	}
	
	/**
	 * Null-safe equivalent of {@code obj1.equals(obj2)}.
	 * 
	 * @param obj1 an object
     * @param obj2 an object to be compared with {@code obj1} for equality
	 * @return {@code true} if the arguments are equal to each other and {@code false} otherwise
	 * 
	 * @see java.util.Objects#equals(Object, Object)
	 * @see java.util.Object#equals(Object)
	 */
	public static boolean equals(Object obj1, Object obj2)
	{
		return (obj1 == obj2) || (obj1 != null && obj1.equals(obj2));
	}

	/**
	 * Returns {@code true} if the arguments are deeply equal to each other and {@code false} otherwise.
	 * 
	 * Two {@code null} values are deeply equal.
	 * If both arguments are primitive arrays, the appropriate {@link Arrays#equals} method is used to determine equality.
	 * If both arguments are arrays of reference types, the {@link Arrays#deepEquals(Object[], Object[]) Arrays.deepEquals} method is used to determine equality.
	 * Otherwise, equality is determined by using the {@link Object#equals equals} method of the first argument.
	 * 
	 * @param obj1 an object
	 * @param obj2 an object to be compared with {@code obj1} for deep equality
	 * @return {@code true} if the arguments are deeply equal to each other and {@code false} otherwise
	 * 
	 * @see java.util.Objects#deepEquals(Object, Object)
	 * @see java.util.Arrays#deepEquals(Object[], Object[])
	 * @see java.util.Arrays#deepEquals0(Object, Object)
	 * @see java.util.Object#equals(Object)
	 */
	public static boolean deepEquals(Object obj1, Object obj2)
	{
		if(obj1 == obj2)
			return true;
		else if(obj1 == null || obj2 == null)
			return false;
		else if(obj1 instanceof Object[] && obj2 instanceof Object[])
			return Arrays.deepEquals((Object[]) obj1, (Object[]) obj2);
		else if(obj1 instanceof byte[] && obj2 instanceof byte[])
			return Arrays.equals((byte[]) obj1, (byte[]) obj2);
		else if(obj1 instanceof boolean[] && obj2 instanceof boolean[])
			return Arrays.equals((boolean[]) obj1, (boolean[]) obj2);
		else if(obj1 instanceof int[] && obj2 instanceof int[])
			return Arrays.equals((int[]) obj1, (int[]) obj2);
		else if(obj1 instanceof long[] && obj2 instanceof long[])
			return Arrays.equals((long[]) obj1, (long[]) obj2);
		else if(obj1 instanceof float[] && obj2 instanceof float[])
			return Arrays.equals((float[]) obj1, (float[]) obj2);
		else if(obj1 instanceof double[] && obj2 instanceof double[])
			return Arrays.equals((double[]) obj1, (double[]) obj2);
		else if(obj1 instanceof short[] && obj2 instanceof short[])
			return Arrays.equals((short[]) obj1, (short[]) obj2);
		else if(obj1 instanceof char[] && obj2 instanceof char[])
			return Arrays.equals((char[]) obj1, (char[]) obj2);
		else
			return obj1.equals(obj2);
	}

	/**
	 * Returns the hash code of a non-{@code null} argument and 0 for a {@code null} argument.
	 *
	 * @param obj an object
	 * @return the hash code of a non-{@code null} argument and 0 for a {@code null} argument
	 * 
	 * @see java.lang.Object#hashCode()
	 * @see java.util.Objects#hashCode(Object)
	 */
	public static int hashCode(Object obj)
	{
		return obj != null ? obj.hashCode() : 0;
	}
	
	/**
	 * Convenience wrapper for {@link Arrays#hashCode}, adding varargs. This can be used to compute a hash code for an object's fields as follows:
	 * {@code Objects.hash(a, b, c)}.
	 * 
	 * @param values
	 * @return
	 * 
	 * @see java.lang.Object#hashCode()
	 * @see java.util.Objects#hash(Object...)
	 */
	public static int hash(Object... values)
	{
		return Arrays.hashCode(values);
	}

	/**
	 * Returns {@code o} if non-null, or throws {@code NullPointerException}.
	 * 
	 * @param o
	 * @return
	 * 
	 * @see java.util.Objects#requireNonNull(Object)
	 */
	public static <T> T requireNonNull(T o)
	{
		if(o == null)
			throw new NullPointerException();
		return o;
	}

	/**
	 * Returns {@code o} if non-null, or throws {@code NullPointerException} with the given detail message.
	 * 
	 * @param o
	 * @param message
	 * @return
	 * 
	 * @see java.util.Objects#requireNonNull(Object, String)
	 */
	public static <T> T requireNonNull(T o, String message)
	{
		if(o == null)
			throw new NullPointerException(message);
		return o;
	}

	/**
	 * Returns "null" for null or {@code o.toString()}.
	 * 
	 * @param o
	 * @return
	 * 
	 * @see java.lang.Object#toString()
	 * @see java.util.Objects#toString(Object)
	 */
	public static String toString(Object o)
	{
		return String.valueOf(o);
	}

	/**
	 * Returns {@code nullString} for null or {@code o.toString()}.
	 * 
	 * @param o
	 * @param nullString
	 * @return
	 * 
	 * @see java.lang.Object#toString()
	 * @see java.util.Objects#toString(Object, String)
	 */
	public static String toString(Object o, String nullString)
	{
		return (o == null) ? nullString : o.toString();
	}

}
