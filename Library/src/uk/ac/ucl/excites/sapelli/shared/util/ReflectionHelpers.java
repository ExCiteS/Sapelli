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
	 * @param field
	 * @param setFinal new state({@code true} field is made {@code final}; {@code false} field is made "non-{@code final}") 
	 * @throws Exception
	 */
	static public void setFinal(Field field, boolean setFinal) throws Exception
	{
		Field modifiersField = Field.class.getDeclaredField("modifiers");
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, (setFinal ?	field.getModifiers() | Modifier.FINAL :
													field.getModifiers() & ~Modifier.FINAL));
		modifiersField.setAccessible(false);
	}

}
