/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

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
		this(new RecordsQuery());
	}
	
	public SingleRecordQuery(RecordsQuery recordsQuery)
	{
		if(recordsQuery == null)
			this.recordsQuery = new RecordsQuery();
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
		
		if(!records.isEmpty())
			// Reduce & return:
			return reduce(records);
		else
			// There are no records, return null
			return null;
	}
	
	/**
	 * @param records list of records to select from, guaranteed non-empty
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
	 */
	public abstract Record acceptExecutor(Executor executor);
	
	/**
	 * 
	 * @author mstevens
	 */
	public interface Executor
	{

		public Record execute(FirstRecordQuery firstRecordQuery);
		
		public Record execute(ExtremeValueRecordQuery extremeValueRecordQuery);

	}
	
}
