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

package uk.ac.ucl.excites.sapelli.collector.control;

import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;

/**
 * @author mstevens
 *
 */
public class FieldWithArguments
{
	
	public final Field field;
	public final FieldParameters arguments;
	
	/**
	 * @param field the field
	 * @throws NullPointerException when the field is null
	 */
	public FieldWithArguments(Field field) throws NullPointerException
	{
		this(field, FieldParameters.EMPTY);
	}
	
	/**
	 * @param field the field
	 * @param arguments arguments passed along by previous field
	 * @throws NullPointerException when either the field or the arguments are null
	 */
	public FieldWithArguments(Field field, FieldParameters arguments) throws NullPointerException
	{
		if(field == null)
			throw new NullPointerException("Field cannot be null");
		if(arguments == null)
			throw new NullPointerException("Arguments cannot be null");
		this.field = field;
		this.arguments = new FieldParameters(arguments); // create a copy!
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof FieldWithArguments)
		{
			FieldWithArguments that = (FieldWithArguments) obj;
			return	this.field.equals(that.field) &&
					this.arguments.equals(that.arguments);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + field.hashCode();
		hash = 31 * hash + arguments.hashCode();
		return hash;
	}
	
}
