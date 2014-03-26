/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db;

import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;

/**
 * Interface for Record storage back-ends
 * 
 * TODO explicit retrieval (and implicit storage) or Schema instances
 * 
 * @author mstevens
 */
public abstract class RecordStore implements Store
{

	/**
	 * @param record - the record to store or update
	 */
	public abstract void store(Record record);

	/**
	 * @param records - the records to store or update
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
	public List<Record> retrieveAllRecords()
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
