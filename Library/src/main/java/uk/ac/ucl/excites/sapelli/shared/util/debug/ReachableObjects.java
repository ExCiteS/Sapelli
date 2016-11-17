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

package uk.ac.ucl.excites.sapelli.shared.util.debug;

import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Debugging utility which prints all objects reachable from a given object.
 *
 * http://stackoverflow.com/questions/23290753/java-objects-reachable-by-strong-or-weak-references
 *
 * @author mstevens
 */
public class ReachableObjects
{
	private static final Field REFERENCE_REFERENT_FIELD = initReferenceReferentField();

	private static Field initReferenceReferentField()
	{
		try
		{
			return Reference.class.getDeclaredField("referent");
		}
		catch(NoSuchFieldException e)
		{
			e.printStackTrace();
		}
		catch(SecurityException e)
		{
			e.printStackTrace();
		}
		return null;
	}

	private Set<Object> softlyReachable;
	private Set<Object> weaklyReachable;
	private Set<Object> phantomReachable;
	private Set<Object> stronglyReachable;

	public ReachableObjects(Object object)
	{
		softlyReachable = new LinkedHashSet<Object>();
		weaklyReachable = new LinkedHashSet<Object>();
		phantomReachable = new LinkedHashSet<Object>();
		stronglyReachable = new LinkedHashSet<Object>();

		try
		{
			collectAllReachableObjects(object, stronglyReachable, "");
		}
		catch(IllegalArgumentException e)
		{
			e.printStackTrace();
		}
		catch(IllegalAccessException e)
		{
			e.printStackTrace();
		}
		softlyReachable.removeAll(weaklyReachable);
		softlyReachable.removeAll(phantomReachable);
		softlyReachable.removeAll(stronglyReachable);

		weaklyReachable.removeAll(softlyReachable);
		weaklyReachable.removeAll(phantomReachable);
		weaklyReachable.removeAll(stronglyReachable);

		phantomReachable.removeAll(softlyReachable);
		phantomReachable.removeAll(weaklyReachable);
		phantomReachable.removeAll(stronglyReachable);
	}

	private void collectAllReachableObjects(Object from, Set<Object> result, String indent) throws IllegalArgumentException, IllegalAccessException
	{
		if(result.contains(from))
		{
			return;
		}
		result.add(from);
		Class<?> c = from.getClass();

		// Detect & loop through arrays:
		if(c.isArray())
		{
			int length = Array.getLength(from);
			for(int i = 0; i < length; i++)
			{
				Object value = Array.get(from, i);
				if(value != null)
					collectAllReachableObjects(value, result, indent);
			}
			return;
		}
		// else: Deal with non-arrays:
		Class<?> leafClass = c;
		while(c != null)
		{
			// System.out.println(indent+"Class "+c);

			Field fields[] = c.getDeclaredFields();
			for(Field field : fields)
			{
				// System.out.println(indent+"Field "+field+" of "+c);

				if(Modifier.isStatic(field.getModifiers()))
				{
					continue;
				}

				boolean wasAccessible = field.isAccessible();
				field.setAccessible(true);
				Object value = field.get(from);
				if(value != null)
				{
					Set<Object> nextResult = stronglyReachable;
					if(field.equals(REFERENCE_REFERENT_FIELD))
					{
						if(leafClass.equals(SoftReference.class))
						{
							nextResult = softlyReachable;
						}
						else if(leafClass.equals(WeakReference.class))
						{
							nextResult = weaklyReachable;
						}
						else if(leafClass.equals(PhantomReference.class))
						{
							nextResult = phantomReachable;
						}
					}
					collectAllReachableObjects(value, nextResult, indent + "  ");
				}
				field.setAccessible(wasAccessible);
			}
			c = c.getSuperclass();
		}
	}

	public List<Object> getSoftlyReachable()
	{
		return new ArrayList<Object>(softlyReachable);
	}

	public List<Object> getWeaklyReachable()
	{
		return new ArrayList<Object>(weaklyReachable);
	}

	public List<Object> getPhantomReachable()
	{
		return new ArrayList<Object>(phantomReachable);
	}

	public List<Object> getStronglyReachable()
	{
		return new ArrayList<Object>(stronglyReachable);
	}

	public void printInfo()
	{
		System.out.println("Soft:");
		printList(getSoftlyReachable());
		System.out.println("Weak:");
		printList(getWeaklyReachable());
		System.out.println("Phantom:");
		printList(getPhantomReachable());
		System.out.println("Strong:");
		printList(getStronglyReachable());
	}

	private void printList(List<Object> list)
	{
		for(Object object : list)
			System.out.println("\t\t" + (object.getClass().isArray() ? "Array:" : "") + object.getClass().getName() + "\t\t" + object.toString());
	}

}