/**
 * 
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
	 * @param recordsQuery
	 */
	public FirstRecordQuery(RecordsQuery recordsQuery)
	{
		super(recordsQuery);
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
	public Record acceptExecutor(Executor executor)
	{
		return executor.execute(this);
	}
	
}
