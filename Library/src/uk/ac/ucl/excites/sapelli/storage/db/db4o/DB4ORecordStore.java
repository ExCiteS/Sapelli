/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db.db4o;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.db4o.DB4OConnector;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;

import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.query.Predicate;

/**
 * DB4O implementation of {@link RecordStore}.
 * 
 * DB4O storage has many known and unknown stability, performance and functionality issues.
 * We have painstakingly tried to work around the most concerning of those, but nevertheless
 * it is essential that we move away from DB4O sooner rather than later.
 * 
 * @author mstevens
 */
public class DB4ORecordStore extends RecordStore
{

	// Statics----------------------------------------------
	static public final String DATABASE_NAME_SUFFIX = "_Data";
	static public final String BACKUP_SUFFIX = "_Backup";
	static public final int ACTIVATION_DEPTH = 40;
	static public final int UPDATE_DEPTH = 40;

	// Dynamics---------------------------------------------
	private ObjectContainer db4o;
	private String filename;
	
	private AutoIncrementDictionary autoIncrementDict;
	
	public DB4ORecordStore(StorageClient client, File folder, String baseFilename) throws Exception
	{
		super(client);
		this.filename = baseFilename + DATABASE_NAME_SUFFIX;
		this.db4o = DB4OConnector.open(DB4OConnector.getFile(folder, filename), Record.class, Schema.class);
		
		// Get or set the AutoIncrementDictionary:
		ObjectSet<AutoIncrementDictionary> resultSet = db4o.query(AutoIncrementDictionary.class);
		if(!resultSet.isEmpty())
			this.autoIncrementDict = resultSet.get(0);
		else
			this.autoIncrementDict = new AutoIncrementDictionary();
	}
	
	@Override
	public void startTransaction()
	{
		// does nothing
	}

	@Override
	public void commitTransaction()
	{
		db4o.commit();
	}

	@Override
	public void rollbackTransaction()
	{
		db4o.rollback();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#doStore(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean doStore(Record record) throws Exception
	{
		boolean insert = !db4o.ext().isStored(record);
		
		// Deal with auto-incrementing primary keys:
		if(record.getSchema().getPrimaryKey() instanceof AutoIncrementingPrimaryKey)
		{
			IntegerColumn autoIncrIDColumn = ((AutoIncrementingPrimaryKey) record.getSchema().getPrimaryKey()).getColumn();
			// Set autoIncr ID:
			if(!autoIncrIDColumn.isValueSet(record)) // equivalent to if(insert)
			{
				autoIncrIDColumn.storeValue(record, autoIncrementDict.getNextID(record.getSchema()));
				// Store the the dictionary:
				db4o.store(autoIncrementDict);
			}
		}
		
		db4o.store(record);
		return insert;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveAllRecords()
	 */
	@Override
	public List<Record> retrieveAllRecords()
	{
		ObjectSet<Record> resultSet = db4o.query(Record.class);
		
		// Activate result records & add to new ArrayList (list returned by DB4O doesn't allow sorting and possibly other things):
		List<Record> result = new ArrayList<Record>();
		for(Record r : resultSet)
		{
			db4o.activate(r, ACTIVATION_DEPTH);
			// Filter out records of internal schemas:
			if(!r.getSchema().isInternal())
				result.add(r);
		}
		return result;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecords(uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery)
	 */
	@Override
	public List<Record> retrieveRecords(final RecordsQuery query)
	{		
		// Query for records:
		ObjectSet<Record> resultSet = db4o.query(new Predicate<Record>()
		{
			private static final long serialVersionUID = 1L;

			public boolean match(Record record)
			{
				return	!record.getSchema().isInternal()													/* filter out records of internal schemas */
						&& (query.isAnySchema() || query.getSourceSchemata().contains(record.getSchema()));	/* Schema check */
			}
		});
		
		// Activate result records, filter by query constraints & add to new ArrayList (list returned by DB4O doesn't allow sorting and possibly other things):
		List<Record> result = new ArrayList<Record>();
		Constraint constraints = query.getConstraints();
		while(resultSet.hasNext())
		{
			Record r = resultSet.next();
			db4o.activate(r, ACTIVATION_DEPTH);
			if(constraints.isValid(r)) // Filter by constraint(s) (for some reason the DB4O query doesn't work if this happens inside the Predicate's match() method)
				result.add(r);
		}
		
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
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecord(uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery)
	 */
	@Override
	public Record retrieveRecord(SingleRecordQuery query)
	{
		// Run the RecordsQuery:
		List<Record> records = retrieveRecords(query.getRecordsQuery());
		
		// Run execute the SingleRecordQuery (reducing the list to 1 record), without re-running the recordsQuery, and then return the result:
		return query.execute(records, false);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveAllDeletableRecords()
	 */
	protected List<Record> retrieveAllDeletableRecords()
	{
		return db4o.query(Record.class); // also includes records of internal schemata
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#doDelete(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public void doDelete(Record record) throws Exception
	{
		db4o.delete(record);
	}

	@Override
	public void finalise()
	{
		db4o.close();
	}

	@Override
	public void backup(File destinationFolder) throws Exception
	{
		db4o.commit();
		db4o.ext().backup(DB4OConnector.getFile(destinationFolder, filename + BACKUP_SUFFIX + "_" + TimeUtils.getTimestampForFileName()).getAbsolutePath());
	}
	
	/**
	 * Helper class which does the book keeping for auto-incrementing primary keys
	 * 
	 * @author mstevens 
	 */
	private class AutoIncrementDictionary extends HashMap<Schema, Long>
	{
		
		private static final long serialVersionUID = 2L;

		public Long getNextID(Schema schema)
		{
			// Check for auto incrementing key:
			if(!(schema.getPrimaryKey() instanceof AutoIncrementingPrimaryKey))
				throw new IllegalArgumentException("Schema must have an auto-incrementing primary key");
			// Next id:
			long next = (containsKey(schema) ? get(schema) : -1l) + 1;
			// Store it:
			put(schema, next); // hash map always keeps the last used id
			// Return it:
			return next;
		}
		
	}

}
