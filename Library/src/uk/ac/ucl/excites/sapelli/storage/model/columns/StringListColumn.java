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
 * @author mstevens
 */
public class StringListColumn extends ListColumn.Simple<String>
{

	private static final long serialVersionUID = 2L;

	/**
	 * Creates a {@link StringListColumn} with minimum length of {@value ListColumn#DEFAULT_MINIMUM_LENGTH} and maximum length of {@value ListColumn#DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional)
	{
		super(name, singleColumn, optional);
	}
	
	/**
	 * Creates a {@link StringListColumn} with minimum length of {@value ListColumn#DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 */
	public StringListColumn(String name, StringColumn singleColumn, boolean optional, int maxLength)
	{
		super(name, singleColumn, optional, maxLength);
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

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
