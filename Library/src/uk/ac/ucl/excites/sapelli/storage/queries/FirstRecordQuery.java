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

/**
 * @author mstevens
 *
 */
public class FirstRecordQuery extends SingleRecordQuery
{

	/**
	 * @param recordsQuery, which will be limited to 1 if it isn't already!
	 */
	public FirstRecordQuery(RecordsQuery recordsQuery)
	{
		super(recordsQuery.getLimit() == 1 ?
				recordsQuery :
				new RecordsQuery(	recordsQuery.getSourceSchemata(),
									recordsQuery.getOrderBy(),
									recordsQuery.isOrderAsc(),
									1, // !!! ensure limit of 1
									recordsQuery.getConstraints()));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery#reduce(java.util.List)
	 */
	@Override
	protected Record reduce(List<Record> records)
	{
		return records.get(0); // return first record
	}

	@Override
	public <R, E extends Throwable> R acceptExecutor(Executor<R, E> executor) throws E
	{
		return executor.execute(this);
	}
	
}
