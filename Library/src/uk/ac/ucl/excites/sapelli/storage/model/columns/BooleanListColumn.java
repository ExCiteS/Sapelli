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

package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * @author mstevens, benelliott
 */
public class BooleanListColumn extends ListColumn.Simple<Boolean>
{

	private static final long serialVersionUID = 2L;
	
	private static BooleanColumn GetSingleColumn()
	{
		return new BooleanColumn(Boolean.class.getSimpleName(), false);
	}

	/**
	 * Creates a {@link BooleanListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param optional
	 * 
	 * @see #ListColumn(String, Column, boolean)
	 */
	public BooleanListColumn(String name, boolean optional)
	{
		super(name, GetSingleColumn(), optional);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param optional
	 * @param defaultValue
	 * 
	 * @see #ListColumn(String, Column, List)
	 */
	public BooleanListColumn(String name, boolean optional, List<Boolean> defaultValue)
	{
		super(name, GetSingleColumn(), optional, defaultValue);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param optional
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, char, char)
	 */
	public BooleanListColumn(String name, boolean optional, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, GetSingleColumn(), optional, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, List, char, char)
	 */
	public BooleanListColumn(String name, boolean optional, List<Boolean> defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, GetSingleColumn(), optional, defaultValue, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param optional
	 * @param maxLength
	 * 
	 * @see #ListColumn(String, Column, boolean, int)
	 */
	public BooleanListColumn(String name, boolean optional, int maxLength)
	{
		super(name, GetSingleColumn(), optional, maxLength);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLength
	 * @param defaultValue
	 * 
	 * @see #ListColumn(String, Column, boolean, int, List)
	 */
	public BooleanListColumn(String name, boolean optional, int maxLength, List<Boolean> defaultValue)
	{
		super(name, GetSingleColumn(), optional, maxLength, defaultValue);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param optional
	 * @param maxLength
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char)
	 */
	public BooleanListColumn(String name, boolean optional, int maxLength, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, GetSingleColumn(), optional, maxLength, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, List, char, char)
	 */
	public BooleanListColumn(String name, boolean optional, int maxLength, List<Boolean> defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, GetSingleColumn(), optional, maxLength, defaultValue, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 */
	public BooleanListColumn(String name, boolean optional, int minLength, int maxLength)
	{
		super(name, GetSingleColumn(), optional, minLength, maxLength);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 */
	public BooleanListColumn(String name, boolean optional, int minLength, int maxLength, List<Boolean> defaultValue)
	{
		super(name, GetSingleColumn(), optional, minLength, maxLength, defaultValue);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, int, char, char, char)
	 */
	public BooleanListColumn(String name, boolean optional, int minLength, int maxLength, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, GetSingleColumn(), optional, minLength, maxLength, serialisationDelimiter, serialisationSeparator);
	}

	/**
	 * @param name
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, int, List, char, char)
	 */
	public BooleanListColumn(String name, boolean optional, int minLength, int maxLength, List<Boolean> defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, GetSingleColumn(), optional, minLength, maxLength, defaultValue, serialisationDelimiter, serialisationSeparator);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
