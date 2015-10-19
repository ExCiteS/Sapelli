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

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * @author mstevens
 *
 */
public class IntegerListColumn extends ListColumn.Simple<Long>
{

	static private final long serialVersionUID = 2L;

	/**
	 * Creates a {@link IntegerListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * 
	 * @see #ListColumn(String, Column, boolean)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional)
	{
		super(name, singleColumn, optional);
	}
	
	/**
	 * Creates a {@link IntegerListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param defaultValue
	 * 
	 * @see #ListColumn(String, Column, List)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, List<Long> defaultValue)
	{
		super(name, singleColumn, optional, defaultValue);
	}
	
	/**
	 * Creates a {@link IntegerListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, char, char, char, Character, Character)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, singleColumn, optional, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param defaultValue
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, List, char, char, char, Character, Character)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, List<Long> defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, singleColumn, optional, defaultValue, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * Creates a {@link IntegerListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * 
	 * @see #ListColumn(String, Column, boolean, int)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int maxLength)
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
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int maxLength, List<Long> defaultValue)
	{
		super(name, singleColumn, optional, maxLength, defaultValue);
	}
	
	/**
	 * Creates a {@link IntegerListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int maxLength, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, singleColumn, optional, maxLength, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, int, List, char, char, char, Character, Character)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int maxLength, List<Long> defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, singleColumn, optional, maxLength, defaultValue, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * Creates a {@link IntegerListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int minLength, int maxLength)
	{
		super(name, singleColumn, optional, minLength, maxLength);
	}
	
	/**
	 * Creates a {@link IntegerListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int minLength, int maxLength, List<Long> defaultValue)
	{
		super(name, singleColumn, optional, minLength, maxLength, defaultValue);
	}
	
	/**
	 * Creates a {@link IntegerListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, int, int, char, char, char, Character, Character)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int minLength, int maxLength, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, singleColumn, optional, minLength, maxLength, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}

	/**
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, int, int, List, char, char, char, Character, Character)
	 */
	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int minLength, int maxLength, List<Long> defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, singleColumn, optional, minLength, maxLength, defaultValue, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
