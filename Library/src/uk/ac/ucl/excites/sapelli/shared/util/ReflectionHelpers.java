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

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * @author mstevens
 *
 */
public final class ReflectionHelpers
{
	
	private ReflectionHelpers() {}
	
	/**
	 * Changes the {@code final} modifier of the given field.
	 * 
	 * Tested and found working on Oracle JVM. Probably also works on Android ART, but *not* on Dalvik.
	 * 
	 * @param field
	 * @param setFinal new state({@code true} field is made {@code final}; {@code false} field is made "non-{@code final}") 
	 * @throws Exception
	 * 
	 * @see {@link Field#getModifiers()}
	 * @see <a href="https://android.googlesource.com/platform/libcore/+/master/libart/src/main/java/java/lang/reflect/Field.java">ART implementation of Field</a>
	 * @see <a href="https://android.googlesource.com/platform/libcore/+/9edf43dfc/libdvm/src/main/java/java/lang/reflect/Field.java">Dalvik implementation of Field</a>
	 * 
	 * @deprecated Doesn't work on Android/Dalvik
	 */
	static public void setFinal(Field field, boolean setFinal) throws Exception
	{
		Field modifiersField = null;
		try
		{
			modifiersField = Field.class.getDeclaredField("modifiers"); // JVM
		}
		catch(NoSuchFieldException nsfe)
		{
			try
			{
				modifiersField = Field.class.getDeclaredField("accessFlags"); // should work on Android ART (not on Dalvik!)
			}
			catch(NoSuchFieldException nsfe2)
			{
				throw new NoSuchFieldException("There is no field called \"modifiers\", nor one called \"accessFlags\"!");
			}
		}
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, (setFinal ?	field.getModifiers() | Modifier.FINAL :
													field.getModifiers() & ~Modifier.FINAL));
		modifiersField.setAccessible(false);
	}

}
