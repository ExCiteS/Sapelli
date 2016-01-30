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

package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;

/**
 * Abstract superclass for {@link RecordsQuery} and {@link SingleRecordQuery}.
 *  
 * @author mstevens
 *
 * @param <R>
 */
public abstract class Query<R>
{
	
	// STATICS-------------------------------------------------------
	static public final int NO_LIMIT = 0;
	
	// DYNAMICS------------------------------------------------------
	/**
	 * Executes the query in Java runtime memory, using a list of records as source
	 * 
	 * @param sourceRecords
	 * @return query result
	 */
	public abstract R execute(List<Record> sourceRecords);
	
	/**
	 * @return the constraints extended with the Source (which is a Constraint which filters records by schema)
	 */
	protected Constraint getInMemoryConstraits()
	{
		return new AndConstraint(getSource(), getConstraints()).reduce();
	}

	/**
	 * @return the source
	 */
	public abstract Source getSource();

	/**
	 * @return the constraints (may be null)
	 */
	public abstract Constraint getConstraints();
	
	/**
	 * @return
	 */
	public boolean hasConstraints()
	{
		return getConstraints().reduce() != null;
	}

	/**
	 * @return the order
	 */
	public abstract Order getOrder();

	/**
	 * @return the limit
	 */
	public abstract int getLimit();
	
	/**
	 * @return whether or not the query is limited to a certain number of resulting records
	 */
	public boolean isLimited()
	{
		return getLimit() > NO_LIMIT;
	}
	
	/**
	 * @return whether or not the query is applies a specifc ordering to the results
	 */
	public boolean isOrdered()
	{
		return getOrder() != Order.UNDEFINED;
	}
	
}
