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

package uk.ac.ucl.excites.sapelli.storage.util;

import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * @author mstevens
 */
public class InvalidValueException extends IllegalArgumentException
{

	private static final long serialVersionUID = 2L;
	
	private final Column<?> column;
	
	public InvalidValueException(String reason, Column<?> column)
	{
		super(reason);
		this.column = column;
	}

	/**
	 * @return the column
	 */
	public Column<?> getColumn()
	{
		return column;
	}
	
}
