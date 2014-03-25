/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;

/**
 * @author mstevens
 *
 */
public abstract class RecordAccess
{
	
	protected final StorageClient client;
	
	public RecordAccess(StorageClient client)
	{
		this.client = client;
	}

	/**
	 * @param record - the record to store
	 */
	public abstract void store(Record record);

	/**
	 * @param records - the records to store
	 */
	public abstract void store(List<Record> records);
	
	/**
	 * @param record - the record to delete
	 */
	public abstract void delete(Record record);

	/**
	 * Retrieve all Records (of any schema)
	 * 
	 * @return
	 */
	public List<Record> retrieveRecords()
	{
		return retrieveRecords(new RecordsQuery());
	}
	
	/**
	 * Retrieve Records of a given Schema
	 * 
	 * @param schema
	 * @return
	 */
	public List<Record> retrieveRecords(Schema schema)
	{
		return retrieveRecords(new RecordsQuery(schema));
	}

	/**
	 * Retrieve records by query
	 * 
	 * @param query
	 * @return
	 */
	public abstract List<Record> retrieveRecords(RecordsQuery query);
	
	/**
	 * Retrieve a single record by SingleRecordQuery 
	 * 
	 * @param query
	 * @return
	 */
	public abstract Record retrieveRecord(SingleRecordQuery query);
	
	/**
	 * Deletes *ALL* records.
	 * USE WITH CARE!
	 */
	public abstract void deleteAllRecords();

}
