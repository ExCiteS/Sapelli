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

import java.lang.reflect.Constructor;

/**
 * Class with helper methods to deal with Class objects.
 * 
 * Uses some "Apache License 2.0"-licensed code from Spring's ClassUtils class.
 * 
 * @see <a href="http://grepcode.com/file_/repo1.maven.org/maven2/org.springframework/spring-core/4.1.6.RELEASE/org/springframework/util/ClassUtils.java">Spring's ClassUtils class</a>
 * 
 * @author mstevens
 * @author Spring framework authors
 */
public final class ClassHelpers
{
	
	private ClassHelpers() {}
	
	/** The package separator character '.' */
	private static final char PACKAGE_SEPARATOR = '.';

	/** The path separator character '/' */
	private static final char PATH_SEPARATOR = '/';

	/**
	 * Given an input class object, return a string which consists of the class's package name as a pathname, i.e., all dots ('.') are replaced by slashes
	 * ('/'). Neither a leading nor trailing slash is added. The result could be concatenated with a slash and the name of a resource and fed directly to
	 * {@code ClassLoader.getResource()}. For it to be fed to {@code Class.getResource} instead, a leading slash would also have to be prepended to the returned value.
	 * 
	 * @param clazz the input class. A {@code null} value or the default (empty) package will result in an empty string ("") being returned.
	 * @return a path which represents the package name
	 * @throws NullPointerException if the given class object is {@code null} (added by mstevens, method used to return "" in this case)
	 * @see ClassLoader#getResource
	 * @see Class#getResource
	 */
	public static String classPackageAsResourcePath(Class<?> clazz) throws NullPointerException
	{
		return classPackage(clazz).replace(PACKAGE_SEPARATOR, PATH_SEPARATOR);
	}
	
	/**
	 * Given an input class object, return a string which consists of the class's package name.
	 * 
	 * @param clazz the input class. A {@code null} value or the default (empty) package will result in an empty string ("") being returned.
	 * @return the name of the package in which the input class resides.
	 * @throws NullPointerException if the given class object is {@code null} (added by mstevens, method used to return "" in this case)
	 */
	public static String classPackage(Class<?> clazz) throws NullPointerException
	{
		if(clazz == null)
			throw new NullPointerException("clazz cannot be null!"); // changed by mstevens (used to return "")
		String className = clazz.getName();
		int packageEndIndex = className.lastIndexOf(PACKAGE_SEPARATOR);
		if(packageEndIndex == -1)
			return "";
		return className.substring(0, packageEndIndex);
	}
	
	/**
	 * @param clazz
	 * @param args
	 * @return
	 */
	public static <C> C callFittingConstructor(Class<C> clazz, Object... args)
	{
		if(args == null)
			args = new Object[0];
		cLoop : for(Constructor<?> constructor : clazz.getConstructors())
		{
			Class<?>[] paramTypes = constructor.getParameterTypes();
			if(args.length != paramTypes.length) // avoid using constructor.getParameterCount() as its only supported since Java 1.8 (even though it is no documented as such, see: https://bugs.openjdk.java.net/browse/JDK-8030785)
				continue;
			int p = 0;
			for(Class<?> paramType : paramTypes)
				if(!paramType.isInstance(args[p++]))
					continue cLoop;
			// Found fitting constructor...
			//	Make it accessible:
			boolean wasAccessible = constructor.isAccessible();
			if(!wasAccessible)
				constructor.setAccessible(true);
			//	Try calling it:
			try
			{
				@SuppressWarnings("unchecked")
				C instance = (C) constructor.newInstance(args);
				return instance;
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				continue cLoop;
			}
			finally
			{
				// Reset accessibility:
				constructor.setAccessible(wasAccessible);
			}
		}
		throw new IllegalArgumentException("No fitting constructor found!");
	}

}
