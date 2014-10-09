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

package uk.ac.ucl.excites.sapelli.shared.db;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class DBConstraintException extends DBException
{

	private static final long serialVersionUID = 2L;

	/**
	 * @param detailMessage
	 * @param cause
	 */
	public DBConstraintException(String detailMessage, Throwable cause)
	{
		super(detailMessage, cause);
	}

	/**
	 * @param detailMessage
	 * @param cause
	 * @param records
	 */
	public DBConstraintException(String detailMessage, Throwable cause, Record... records)
	{
		super(detailMessage, cause, records);
	}

	/**
	 * @param detailMessage
	 */
	public DBConstraintException(String detailMessage)
	{
		super(detailMessage);
	}

	/**
	 * @param detailMessage
	 * @param records
	 */
	public DBConstraintException(String detailMessage, Record... records)
	{
		super(detailMessage, records);
	}

	/**
	 * @param cause
	 */
	public DBConstraintException(Throwable cause)
	{
		super(cause);
	}

	/**
	 * @param cause
	 * @param records
	 */
	public DBConstraintException(Throwable cause, Record... records)
	{
		super(cause, records);
	}

}
