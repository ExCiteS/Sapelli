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

/**
 * Query resulting in a single record instance
 * 
 * @author mstevens
 */
public abstract class SingleRecordQuery
{

	private final RecordsQuery recordsQuery;
	
	public SingleRecordQuery()
	{
		this((RecordsQuery) null);
	}
	
	public SingleRecordQuery(Schema sourceSchema)
	{
		this(new RecordsQuery(Source.From(sourceSchema)));
	}
	
	public SingleRecordQuery(RecordsQuery recordsQuery)
	{
		if(recordsQuery == null)
			this.recordsQuery = RecordsQuery.ALL; // query across all schemata without constraints
		else
			this.recordsQuery = recordsQuery;
	}
	
	/**
	 * Executes the query in Java runtime memory, using a list of records as source.
	 * The list will first be filtered/sorted/limited by means of the {@code recordsQuery}.
	 * 
	 * @param sourceRecords
	 * @return
	 */
	public Record execute(List<Record> sourceRecords)
	{
		return execute(sourceRecords, true);
	}
	
	/**
	 * Executes the query in Java runtime memory, using a list of records as source.
	 * When {@code executeRecordsQuery} is {@code true} the {@code recordsQuery} will be
	 * applied to filter/sort/limit the source records, if it is {@code false} it is
	 * assumed that the source records have already been appropriately filtered/sorted/limited
	 * and the {@code recordsQuery} will not be applied to them before reduction.   
	 * 
	 * @param sourceRecords
	 * @param executeRecordQuery
	 * @return the record that results from the query, or null if there was no matching record
	 */
	public Record execute(List<Record> sourceRecords, boolean executeRecordQuery)
	{
		List<Record> records = sourceRecords;
	
		// Apply records query:
		if(executeRecordQuery)
			records = recordsQuery.execute(records);
		
		if(records != null && !records.isEmpty())
		{
			if(records.size() == 1)
				return records.get(0);
			else
			{
				// Sort:
				recordsQuery.order.sort(records);
				
				 // Reduce & return:
				return reduce(records);
			}
		}
		else
			// There are no records, return null
			return null;
	}
	
	/**
	 * @param records list of records to select from, guaranteed non-null & non-empty
	 * @return
	 */
	protected abstract Record reduce(List<Record> records);

	/**
	 * @return the recordsQuery
	 */
	public RecordsQuery getRecordsQuery()
	{
		return recordsQuery;
	}
	
	/**
	 * @param executor
	 * @return
	 * @throws E
	 */
	public abstract <R, E extends Throwable> R acceptExecutor(Executor<R, E> executor) throws E;
	
	/**
	 * @author mstevens
	 *
	 * @param <R> some result
	 * @param <E> a Throwable
	 */
	public interface Executor<R, E extends Throwable>
	{

		/**
		 * @param firstRecordQuery
		 * @return
		 * @throws E
		 */
		public R execute(FirstRecordQuery firstRecordQuery) throws E;
		
		/**
		 * @param extremeValueRecordQuery
		 * @return
		 * @throws E
		 */
		public R execute(ExtremeValueRecordQuery extremeValueRecordQuery) throws E;

	}
	
}
