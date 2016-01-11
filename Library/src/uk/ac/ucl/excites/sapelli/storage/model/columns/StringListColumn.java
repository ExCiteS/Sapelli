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
 * @author mstevens
 */
public class StringListColumn extends ListColumn.Simple<String>
{

	private static final long serialVersionUID = 2L;

	/**
	 * Creates a {@link StringListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * 
	 * @see #ListColumn(String, Column, boolean)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional)
	{
		super(name, singleColumn, optional);
	}
	
	/**
	 * Creates a {@link StringListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param defaultValue
	 * 
	 * @see #ListColumn(String, Column, List)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, List<String> defaultValue)
	{
		super(name, singleColumn, optional, defaultValue);
	}
	
	/**
	 * Creates a {@link StringListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, char, char)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, singleColumn, optional, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, List, char, char)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, List<String> defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, singleColumn, optional, defaultValue, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * Creates a {@link StringListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * 
	 * @see #ListColumn(String, Column, boolean, int)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int maxLength)
	{
		super(name, singleColumn, optional, maxLength);
	}
	
	/**
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param defaultValue
	 * 
	 * @see #ListColumn(String, Column, boolean, int, List)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int maxLength, List<String> defaultValue)
	{
		super(name, singleColumn, optional, maxLength, defaultValue);
	}
	
	/**
	 * Creates a {@link StringListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int maxLength, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, singleColumn, optional, maxLength, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, List, char, char)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int maxLength, List<String> defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, singleColumn, optional, maxLength, defaultValue, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * Creates a {@link StringListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int minLength, int maxLength)
	{
		super(name, singleColumn, optional, minLength, maxLength);
	}
	
	/**
	 * Creates a {@link StringListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int minLength, int maxLength, List<String> defaultValue)
	{
		super(name, singleColumn, optional, minLength, maxLength, defaultValue);
	}
	
	/**
	 * Creates a {@link StringListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, int, char, char)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int minLength, int maxLength, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, singleColumn, optional, minLength, maxLength, serialisationDelimiter, serialisationSeparator);
	}

	/**
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, int, List, char, char)
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int minLength, int maxLength, List<String> defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, singleColumn, optional, minLength, maxLength, defaultValue, serialisationDelimiter, serialisationSeparator);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
