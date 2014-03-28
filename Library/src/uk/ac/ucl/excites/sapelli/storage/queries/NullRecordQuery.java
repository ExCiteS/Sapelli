/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Dummy subclass of SingleRecordQuery which always returns null. Use with care!
 * 
 * @author mstevens
 */
public class NullRecordQuery extends SingleRecordQuery
{

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery#reduce(java.util.List)
	 */
	@Override
	protected Record reduce(List<Record> records)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery#acceptExecutor(uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery.Executor)
	 */
	@Override
	public Record acceptExecutor(Executor executor)
	{
		return null;
	}

}
