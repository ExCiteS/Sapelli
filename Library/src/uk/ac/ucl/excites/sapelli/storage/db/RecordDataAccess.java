/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.SelectionQuery;

/**
 * @author mstevens
 *
 */
public abstract class RecordDataAccess
{
	
	protected StorageClient client;
	
	public RecordDataAccess(StorageClient client)
	{
		this.client = client;
	}

	/**
	 * @param record - the record to store
	 */
	public abstract void store(Record record);

	/**
	 * @param record - the record to delete
	 */
	public abstract void delete(Record record);

	/**
	 * Retrieve all Records
	 * 
	 * @return
	 */
	public List<Record> retrieveRecords()
	{
		return retrieveRecords((SelectionQuery) null);
	}
	
	/**
	 * Retrieve all records matching the given selection query (which may be null, in which case no restrictions apply)
	 * 
	 * @param query
	 * @return
	 */
	public abstract List<Record> retrieveRecords(SelectionQuery query);

	/**
	 * Retrieve Records of a given Schema
	 * 
	 * @param schema
	 * @return
	 */
	public List<Record> retrieveRecords(Schema schema)
	{
		return retrieveRecords(schema, null);
	}
	
	/**
	 * Retrieve Records of a given Schema which also match the given selection query (which may be null, in which case no restrictions apply)
	 * 
	 * @param schema
	 * @return
	 */
	public abstract List<Record> retrieveRecords(Schema schema, SelectionQuery query);
		
	/**
	 * Deletes *ALL* records.
	 * USE WITH CARE!
	 */
	public abstract void deleteAllRecords();

}
