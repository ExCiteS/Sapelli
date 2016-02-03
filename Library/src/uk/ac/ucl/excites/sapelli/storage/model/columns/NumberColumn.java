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

package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.util.InvalidValueException;

/**
 * @author mstevens
 *
 * @param <N>
 */
public abstract class NumberColumn<N extends Number> extends ComparableColumn<N>
{

	private static final long serialVersionUID = 2L;

	/**
	 * @param name
	 * @param optional
	 * @param defaultValue
	 * @throws IllegalArgumentException in case the given name is invalid
	 * @throws InvalidValueException in case the given defaultValue is invalid
	 */
	public NumberColumn(String name, boolean optional, N defaultValue) throws IllegalArgumentException, InvalidValueException
	{
		super(name, optional, defaultValue);
	}

}
