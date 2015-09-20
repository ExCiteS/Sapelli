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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.util.IncompletePrimaryKeyException;

public abstract class RecordValueSet<CS extends ColumnSet> extends ValueSet<CS>
{

	private static final long serialVersionUID = 2L;

	/**
	 * @param columnSet
	 * @param serialisedValues
	 * @throws NullPointerException
	 * @throws IOException
	 */
	public RecordValueSet(CS columnSet, byte[] serialisedValues) throws NullPointerException, IOException
	{
		super(columnSet, serialisedValues);
	}

	/**
	 * @param columnSet
	 * @param values
	 */
	public RecordValueSet(CS columnSet, Object... values)
	{
		super(columnSet, values);
	}

	/**
	 * @param columnSet
	 * @param serialisedValues
	 * @throws Exception
	 */
	public RecordValueSet(CS columnSet, String serialisedValues) throws Exception
	{
		super(columnSet, serialisedValues);
	}

	/**
	 * @param columnSet
	 */
	public RecordValueSet(CS columnSet)
	{
		super(columnSet);
	}

	/**
	 * @param another
	 */
	public RecordValueSet(ValueSet<CS> another)
	{
		super(another);
	}

	/**
	 * Shared method of {@link Record} and {@link RecordReference}.
	 * 
	 * @return a query that looks for this record
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key have not all been assigned a value
	 */
	public abstract SingleRecordQuery getRecordQuery() throws IncompletePrimaryKeyException;

	/**
	 * Shared method of {@link Record} and {@link RecordReference}.
	 * 
	 * @return a Constraint
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key have not all been assigned a value
	 */
	public abstract Constraint getRecordQueryConstraint() throws IncompletePrimaryKeyException;

}