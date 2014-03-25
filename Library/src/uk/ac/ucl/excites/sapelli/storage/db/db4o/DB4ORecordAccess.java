/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db.db4o;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.RecordAccess;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;

import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.query.Predicate;

/**
 * DB4O implementation of {@link RecordAccess}.
 * 
 * @author mstevens
 */
public class DB4ORecordAccess extends RecordAccess
{

	// Statics----------------------------------------------
	static protected final String TAG = "DB4ODataAccess";
	static public final int ACTIVATION_DEPTH = 40;
	static public final int UPDATE_DEPTH = 40;


	// Dynamics---------------------------------------------
	private ObjectContainer db;
	
	public DB4ORecordAccess(ObjectContainer db, StorageClient client)
	{
		super(client);
		this.db = db;
	}
	
	public void commit()
	{
		db.commit();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordDataAccess#store(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public void store(Record record)
	{
		db.store(record);
		db.commit();
	}
	
	@Override
	public void store(List<Record> records)
	{
		for(Record r : records)
			db.store(r);
		db.commit();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordDataAccess#delete(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public void delete(Record record)
	{
		db.delete(record);
		db.commit();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordDataAccess#retrieveRecords()
	 */
	@Override
	public List<Record> retrieveRecords()
	{
		List<Record> result = db.query(Record.class);
		for(Record r : result)
			db.activate(r, ACTIVATION_DEPTH);
		return result;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordDataAccess#deleteAllRecords()
	 */
	@Override
	public void deleteAllRecords()
	{
		List<Record> result = db.query(Record.class);
		for(Record r : result)
			db.delete(r);
		db.commit();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordDataAccess#retrieveRecords(uk.ac.ucl.excites.sapelli.storage.queries.Query)
	 */
	@Override
	public List<Record> retrieveRecords(final RecordsQuery query)
	{
		// Query for records:
		ObjectSet<Record> result = db.query(new Predicate<Record>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Record record)
			{
				return	(query.isAnySchema() || record.getSchema().equals(query.getSourceSchema())) /* Schema check */
						&& query.getConstraints().select(record);									/* Filter by constraint(s) */ 
			}
		});
		
		// Activate result records:
		for(Record r : result)
			db.activate(r, ACTIVATION_DEPTH);
		
		// Sort result:
		query.sort(result);
		
		// Apply limit if necessary & return result:
		int limit = query.getLimit();
		if(limit != RecordsQuery.NO_LIMIT && result.size() > limit)
			return result.subList(0, limit);
		else
			return result;		
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordAccess#retrieveRecord(uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery)
	 */
	@Override
	public Record retrieveRecord(SingleRecordQuery query)
	{
		// Run the RecordsQuery:
		List<Record> records = retrieveRecords(query.getRecordsQuery());
		
		// Run execute the SingleRecordQuery (reducing the list to 1 record), without re-running the recordsQuery, return the result:
		return query.execute(records, false);
	}

}
