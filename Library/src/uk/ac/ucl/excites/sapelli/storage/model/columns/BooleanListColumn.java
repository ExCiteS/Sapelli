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
	 * @param singleColumn
	 * @param optional
	 */
	public BooleanListColumn(String name, boolean optional)
	{
		super(name, GetSingleColumn(), optional);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
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
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public BooleanListColumn(String name, boolean optional, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, GetSingleColumn(), optional, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 */
	public BooleanListColumn(String name, boolean optional, int maxLength)
	{
		super(name, GetSingleColumn(), optional, maxLength);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
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
	public BooleanListColumn(String name, boolean optional, int maxLength, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, GetSingleColumn(), optional, maxLength, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * Creates a {@link BooleanListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
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
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public BooleanListColumn(String name, boolean optional, int minLength, int maxLength, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, GetSingleColumn(), optional, minLength, maxLength, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
