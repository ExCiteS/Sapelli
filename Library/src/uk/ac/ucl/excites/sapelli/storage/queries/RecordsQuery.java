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

package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;

public class RecordsQuery
{

	// STATICS-------------------------------------------------------
	static public final int NO_LIMIT = 0;
	static public final Constraint[] NO_CONSTRAINTS = null;
	
	/**
	 * Query for all records of any known schema, returned in undefined order
	 */
	static public final RecordsQuery ALL = new RecordsQuery(Source.ANY, Order.UNDEFINED, NO_LIMIT, NO_CONSTRAINTS);
	
	// DYNAMICS------------------------------------------------------
	/*package*/ final Source source;
	/*package*/ final Constraint constraints;
	/*package*/ final Order order;
	/*package*/ final int limit;
	
	/**
	 * Query all records of the given schema
	 * 
	 * @param schema
	 */
	public RecordsQuery(Schema schema)
	{
		this(Source.From(schema), Order.UNDEFINED, NO_LIMIT, NO_CONSTRAINTS);
	}
	
	/**
	 * Query which is defined only by the source
	 * 
	 * @param source
	 */
	public RecordsQuery(Source source)
	{
		this(source, Order.UNDEFINED, NO_LIMIT, NO_CONSTRAINTS);
	}
	
	/**
	 * Query all records of any known schema and order them
	 * 
	 * @param source
	 * @param order
	 */
	public RecordsQuery(Order order)
	{
		this(Source.ANY, order, NO_LIMIT, NO_CONSTRAINTS);
	}
	
	/**
	 * Query all records of any known schema which meet the constraints
	 * 
	 * @param constraints
	 */
	public RecordsQuery(Constraint... constraints)
	{
		this(Source.ANY, Order.UNDEFINED, NO_LIMIT, constraints);
	}
	
	/**
	 * Query with given schema and order
	 * 
	 * @param schema
	 * @param order
	 */
	public RecordsQuery(Schema schema, Order order)
	{
		this(Source.From(schema), order, NO_LIMIT, NO_CONSTRAINTS);
	}
	
	/**
	 * Query with defined source and order
	 * 
	 * @param source
	 * @param order
	 */
	public RecordsQuery(Source source, Order order)
	{
		this(source, order, NO_LIMIT, NO_CONSTRAINTS);
	}
	
	/**
	 * Query for given schema and constraints
	 * 
	 * @param schema
	 * @param constraints
	 */
	public RecordsQuery(Schema schema, Constraint... constraints)
	{
		this(Source.From(schema), constraints);
	}
	
	/**
	 * Query with defined source and constraints
	 * 
	 * @param source
	 * @param constraints
	 */
	public RecordsQuery(Source source, Constraint... constraints)
	{
		this(source, Order.UNDEFINED, NO_LIMIT, constraints);
	}
	
	/**
	 * Query with given schema, limit and constraints
	 * 
	 * @param schema
	 * @param limit
	 * @param constraints
	 */
	public RecordsQuery(Schema schema, int limit, Constraint... constraints)
	{
		this(Source.From(schema), Order.UNDEFINED, limit, constraints);
	}
	
	/**
	 * Query with defined source, limit and constraints
	 * 
	 * @param source
	 * @param limit
	 * @param constraints
	 */
	public RecordsQuery(Source source, int limit, Constraint... constraints)
	{
		this(source, Order.UNDEFINED, limit, constraints);
	}
	
	/**
	 * Query with given schema, order and limit
	 * 
	 * @param schema
	 * @param order
	 * @param limit
	 */
	public RecordsQuery(Schema schema, Order order, int limit)
	{
		this(Source.From(schema), order, limit, NO_CONSTRAINTS);
	}
	
	/**
	 * Query with defined source, order and limit
	 * 
	 * @param source
	 * @param order
	 * @param limit
	 */
	public RecordsQuery(Source source, Order order, int limit)
	{
		this(source, order, limit, NO_CONSTRAINTS);
	}
	
	/**
	 * Query for given schema, order and constraints
	 * 
	 * @param schema
	 * @param constraints
	 */
	public RecordsQuery(Schema schema, Order order, Constraint... constraints)
	{
		this(Source.From(schema), order, constraints);
	}
	
	/**
	 * Query with defined source, order and constraints
	 * 
	 * @param source
	 * @param order
	 * @param constraints
	 */
	public RecordsQuery(Source source, Order order, Constraint... constraints)
	{
		this(source, order, NO_LIMIT, constraints);
	}
	
	/**
	 * Query with defined source, order, limit and constraints
	 * 
	 * @param source
	 * @param order
	 * @param limit
	 * @param constraints
	 */
	public RecordsQuery(Source source, Order order, int limit, Constraint... constraints)
	{
		this.source = source == null ? Source.ANY : source;
		this.constraints = constraints != null && constraints.length == 1 ?
							Constraint.Reduce(constraints[0]) : 
							new AndConstraint(constraints).reduce(); // can deal with the array or one of its elements being null, nested ANDs will be flattened
		this.order = order == null ? Order.UNDEFINED : order;
		if(limit < NO_LIMIT)
			throw new IllegalArgumentException("Limit must be positive, or 0 (meaning there is no limit)");
		this.limit = limit;
	}
	
	/**
	 * Executes the query in Java runtime memory, using a list of records as source
	 * 
	 * @param sourceRecords
	 */
	public List<Record> execute(List<Record> sourceRecords)
	{
		List<Record> records = sourceRecords;
		
		// Apply constraints:
		Constraint inMemoryConstraints = getInMemoryConstraits();
		if(inMemoryConstraints != null)
			records = inMemoryConstraints.filter(records);
		
		// Sort:
		order.sort(records);
		
		// Limit:
		if(limit != NO_LIMIT)
			records = records.subList(0, limit);
		
		return records;
	}
	
	private Constraint getInMemoryConstraits()
	{
		return new AndConstraint(source, constraints).reduce(); // Source is a Constraint which filters records by schema!
	}

	/**
	 * @return the source
	 */
	public Source getSource()
	{
		return source;
	}

	/**
	 * @return the constraints (may be null)
	 */
	public Constraint getConstraints()
	{
		return constraints;
	}
	
	/**
	 * @return
	 */
	public boolean hasConstraints()
	{
		return constraints != null;
	}

	/**
	 * @return the order
	 */
	public Order getOrder()
	{
		return order;
	}

	/**
	 * @return the limit
	 */
	public int getLimit()
	{
		return limit;
	}
	
	/**
	 * @return where or not the query is limited to a certain number of resulting records
	 */
	public boolean isLimited()
	{
		return limit > NO_LIMIT;
	}

}
