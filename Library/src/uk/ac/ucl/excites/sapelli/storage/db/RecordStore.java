/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db;

import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;

/**
 * Abstract superclass for Record storage back-ends
 * 
 * Note: Records of internal schemata are not to be stored/retrieved directly. 
 * 
 * TODO explicit retrieval (and implicit storage) or Schema instances
 * 
 * @author mstevens
 */
public abstract class RecordStore implements Store
{

	protected StorageClient client;
	
	public RecordStore(StorageClient client)
	{
		this.client = client;
	}
	
	/**
	 * @param record - the record to store or update; records of internal schemata will be rejected
	 */
	public void store(Record record)
	{
		// Checks:
		if(record == null)
			return; //throw new NullPointerException("Cannot store null record");
		if(record.getSchema().isInternal())
			throw new IllegalArgumentException("Cannot store record of internal schema directly.");
		// Store:
		storeNonInternal(record);
	}

	/**
	 * @param record - the record to store or update; can be assumed to be non-null and not of an internal schema
	 */
	protected abstract void storeNonInternal(Record record);
	
	/**
	 * @param records - the records to store or update
	 */
	public void store(List<Record> records)
	{
		for(Record r : records)
			store(r);
	}
	
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
