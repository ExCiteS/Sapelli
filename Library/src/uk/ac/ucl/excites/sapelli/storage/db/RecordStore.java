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

package uk.ac.ucl.excites.sapelli.storage.db;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.shared.db.StoreBackuper;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBConstraintException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBPrimaryKeyException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;

/**
 * Abstract superclass for Record storage back-ends
 * 
 * Note: Records of internal schemata are not to be stored/retrieved directly. 
 * 
 * @author mstevens
 */
public abstract class RecordStore implements Store
{

	// STATIC -----------------------------------------------------------------
	static public final String DATABASE_NAME_SUFFIX = "-RecordStore";
	static public final String BACKUP_SUFFIX = "_Backup_"; // to be followed by a timestamp
	
	/**
	 * The Record should be INSERTed if it is not stored yet, or UPDATEed(/replaced) if it is.
	 * If an INSERT or UPDATE takes place the lastStoredAt value must be set to the current time.
	 */
	static protected final int ACTION_INSERT_OR_UPDATE = 0;
	
	/**
	 * The Record should be INSERTed if it is not stored yet, but UPDATE of existing record is not allowed.
	 * If an INSERT takes place the lastStoredAt value must be set to the current time. 
	 */
	static protected final int ACTION_INSERT_ONLY = 1;
	
	/**
	 * The Record should be UPDATEd if it exist, but should not be INSERTed if it does not.
	 * If an UPDATE takes place the lastStoredAt value must be set to the current time.
	 */
	static protected final int ACTION_UPDATE_ONLY = 2;
	
	/**
	 * The Record should be UPDATEd if it exist, but should not be INSERTed if it does not.
	 * The lastStoredAt value should *not* be changed.
	 */
	static protected final int ACTION_UPDATE_ONLY_EXCEPT_LSA = 3;
	
	/**
	 * The Record did not yet exist in the RecordStore,
	 * and it will have been INSERTed if that was allowed.  
	 */
	static protected final int RESULT_NEEDED_INSERT = 0;
	
	/**
	 * The Record already existed in the RecordStore,
	 * and it will have been UPDATEd if that was allowed.
	 */
	static protected final int RESULT_NEEDED_UPDATE = 1;
	
	/**
	 * The Record already existed in the RecordStore, with the exact same values (except perhaps LSA),
	 * hence it was neither INSERTed nor UPDATEd.
	 */
	static protected final int RESULT_NEEDED_NO_ACTION = 2;
	
	static protected Long Now()
	{
		return Long.valueOf(System.currentTimeMillis());
	}
	
	static protected Long GetLastStoredAt(Record record)
	{
		if(record != null && !record.getSchema().isInternal())
			return Schema.COLUMN_LAST_STORED_AT.retrieveValue(record);
		else
			return null;
	}
	
	static protected void SetLastStoredAt(Record record, Long lastStoredAt)
	{
		if(record != null && !record.getSchema().isInternal())
			Schema.COLUMN_LAST_STORED_AT.storeValue(record, lastStoredAt);
		// else: do nothing
	}
	
	// DYNAMIC ----------------------------------------------------------------
	protected StorageClient client;
	
	/**
	 * Few DBMSs support nested transactions, but this counter allows us to simulate them,
	 * which enables us to keep code that deals with transactions simpler. 
	 */
	private int openTransactions = 0;
	
	/**
	 * {@link Stack} with {@link List}s of {@link RollbackTask}s to execute upon roll-back of transaction(s)
	 */
	private final Stack<List<RollbackTask>> rollbackTasks;
	
	/**
	 * @param client
	 * @param useRollbackTasks whether or not the subclass will/might make use of roll-back tasks 
	 */
	public RecordStore(StorageClient client, boolean useRollbackTasks)
	{
		this.client = client;
		rollbackTasks = useRollbackTasks ? new Stack<List<RollbackTask>>() : null;
	}

	/**
	 * Starts a new transaction.
	 * 
	 * @throws DBException
	 */
	public void startTransaction() throws DBException
	{
		try
		{
			doStartTransaction();
		}
		catch(DBException dbE)
		{
			throw dbE;
		}
		openTransactions++; // !!!
		if(rollbackTasks != null)
			rollbackTasks.push(Collections.<RollbackTask> emptyList()); // will be replaced by proper ArrayList when needed
	}
	
	protected abstract void doStartTransaction() throws DBException;
	
	/**
	 * Add task to be executed upon roll-back of (all) open transaction(s).
	 * This can be used to perform in memory (as in non-DB) operations to make the (Java) runtime state reflect
	 * the state of the DB in the event of a roll-back. 
	 * 
	 * @param task
	 * @throws DBException
	 */
	protected void addRollbackTask(RollbackTask task) throws DBException
	{
		if(rollbackTasks == null)
			throw new DBException("This RecordStore implementation does not use rollback tasks.");
		if(rollbackTasks.isEmpty()) // equivalent to: if(!isInTransaction())
			throw new DBException("Cannot add a rollback task unless there is at least one open transaction!");
		if(task == null)
			throw new NullPointerException("Task cannot be null");
		addRollbackTasks(Collections.singleton(task));
	}
	
	/**
	 * @param tasks to add to list for current transaction (assumed to be non-empty)
	 */
	private void addRollbackTasks(Collection<RollbackTask> tasks)
	{
		if(rollbackTasks.peek().isEmpty())
		{	// Replace immutable empty list with ArrayList:
			rollbackTasks.pop();
			rollbackTasks.push(new ArrayList<RollbackTask>(tasks.size()));
		}
		rollbackTasks.peek().addAll(tasks);
	}
	
	/**
	 * Commits the current transaction.

	 * @throws DBException
	 */
	public void commitTransaction() throws DBException
	{
		if(openTransactions > 0)
		{
			try
			{
				doCommitTransaction();
			}
			catch(DBException dbE)
			{
				throw dbE;
			}
			openTransactions--; // !!!
			
			// Deal with roll-back tasks if needed:
			if(rollbackTasks != null)
			{
				// Get tasks for the committed transaction:
				List<RollbackTask> tasks = rollbackTasks.pop();
				// If there is another ("outer-more") transaction and the committed transaction had at least 1 task...
				if(isInTransaction() && !tasks.isEmpty())
					addRollbackTasks(tasks); // move task(s) to outer-more transaction
			}
		}
		//else
		//	System.err.println("Warning: there is no open transaction to commit!");
	}
	
	protected abstract void doCommitTransaction() throws DBException;
	
	/**
	 * Rolls back all(!) transactions.
	 * 
	 * @throws DBException
	 */
	public void rollbackTransactions() throws DBException
	{
		//if(openTransactions == 0)
		//	System.err.println("Warning: there is no open transaction to roll back!");
		while(openTransactions > 0)
			rollbackTransaction();
	}
	
	/**
	 * Rolls back the current transaction.
	 * 
	 * @throws DBException
	 */
	private void rollbackTransaction() throws DBException
	{
		if(openTransactions <= 0)
			return; // System.err.println("Warning: there is no open transaction to roll back!");
		// Perform actual roll-back:
		doRollbackTransaction();
		// Reduce number of open transactions:
		openTransactions--;
		// Run RollbackTasks associated with the rolled-back transaction:
		if(rollbackTasks != null)
			for(RollbackTask task : rollbackTasks.pop())
				task.run();
	}
	
	protected abstract void doRollbackTransaction() throws DBException;

	/**
	 * @return whether or not there is an open transaction
	 */
	public boolean isInTransaction()
	{
		return openTransactions > 0;
	}
	
	/**
	 * @return the number of currently open (possibly simulated) transactions
	 */
	protected int numberOfOpenTransactions()
	{
		return openTransactions;
	}
	
	/**
	 * Verifies if a given record can be stored.
	 * 
	 * @param record
	 * @throws NullPointerException when the record is null
	 * @throws IllegalArgumentException when the record cannot be stored
	 */
	public void checkStorable(Record record) throws NullPointerException, IllegalArgumentException
	{
		if(record == null)
			throw new NullPointerException("Cannot store a null record");
		else if(record.getSchema().isInternal()) // records of "internal" schemata cannot be stored directly
			throw new IllegalArgumentException(String.format("Record (%s) cannot be inserted, because it is of an internal schema!", record.toString(false)));
	}
	
	/**
	 * Stores a single record, if it already exists it is updated.
	 * Note that this method does not start a new transaction. If this is a desired the client code should take care of that by first calling {@link #startTransaction()}.
	 * However, if an error occurs any open transaction will be rolled back!
	 * 
	 * @param record - the record to store (i.e. insert or update); records of internal schemata will be rejected
	 * @throws DBConstraintException when a table/index constraint is violated
	 * @throws DBException in case of a database problem
	 * @throws NullPointerException when the record is null
	 * @throws IllegalArgumentException when the record cannot be stored
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	public void store(Record record) throws DBException, IllegalArgumentException, IllegalStateException
	{
		store(record, ACTION_INSERT_OR_UPDATE);
	}
	
	/**
	 * Insert a single record, if it already exists a DuplicateException will be thrown.
	 * Note that this method does not start a new transaction. If this is a desired the client code should take care of that by first calling {@link #startTransaction()}.
	 * However, if an error occurs any open transaction will be rolled back!
	 * 
	 * @param record - the record to insert; records of internal schemata will be rejected
	 * @throws DBPrimaryKeyException when the record already exists in the database
	 * @throws DBConstraintException when a table/index constraint is violated
	 * @throws DBException in case of another database problem
	 * @throws NullPointerException when the record is null
	 * @throws IllegalArgumentException when the record cannot be stored
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	public void insert(Record record) throws DBPrimaryKeyException, DBConstraintException, DBException, NullPointerException, IllegalArgumentException, IllegalStateException
	{
		store(record, ACTION_INSERT_ONLY);
	}
	
	/**
	 * @param record
	 * @throws DBPrimaryKeyException
	 * @throws DBConstraintException
	 * @throws DBException
	 * @throws NullPointerException
	 * @throws IllegalArgumentException
	 * @throws IllegalStateException
	 */
	public void update(Record record) throws DBPrimaryKeyException, DBConstraintException, DBException, NullPointerException, IllegalArgumentException, IllegalStateException
	{
		update(record, true);
	}
	
	/**
	 * Updates a single record, if it does not exist a DuplicateException will be thrown.
	 * Note that this method does not start a new transaction. If this is a desired the client code should take care of that by first calling {@link #startTransaction()}.
	 * However, if an error occurs any open transaction will be rolled back!
	 * 
	 * @param record - the record to update; records of internal schemata will be rejected
	 * @param updateLastStoredAt whether or not to update the lastStoredAt value
	 * @throws DBPrimaryKeyException when the record does not exist in the database
	 * @throws DBConstraintException when a table/index constraint is violated
	 * @throws DBException in case of another database problem
	 * @throws NullPointerException when the record is null
	 * @throws IllegalArgumentException when the record cannot be stored
	 * @throws IllegalStateException
	 */
	public void update(Record record, boolean updateLastStoredAt) throws DBPrimaryKeyException, DBConstraintException, DBException, NullPointerException, IllegalArgumentException, IllegalStateException
	{
		store(record, updateLastStoredAt ? ACTION_UPDATE_ONLY : ACTION_UPDATE_ONLY_EXCEPT_LSA);
	}
	
	/**
	 * Helper method to store a record using the given action(s).
	 * 
	 * @param record - the Record object to store
	 * @param action - an {@code int} representing the allowed storage action(s), possible values: {@link #ACTION_INSERT_OR_UPDATE}, {@link #ACTION_INSERT_ONLY}, {@link #ACTION_UPDATE_ONLY} & {@link #ACTION_UPDATE_ONLY_EXCEPT_LSA}
	 * @throws DBPrimaryKeyException
	 * @throws DBConstraintException
	 * @throws DBException in case of another database problem
	 * @throws NullPointerException when the record is null
	 * @throws IllegalArgumentException when the record cannot be stored
	 * @throws IllegalStateException
	 */
	protected void store(Record record, int action) throws DBPrimaryKeyException, DBConstraintException, DBException, NullPointerException, IllegalArgumentException, IllegalStateException
	{
		checkStorable(record); // throws NullPointerException, IllegalArgumentException
		int result = -1;
		try
		{
			result = doStore(record, action);
		}
		catch(DBException e)
		{
			rollbackTransactions(); // !!!
			throw e;
		}
		report(record, action, result); // reports to client or throws DBPrimaryKeyException
	}
	
	/**
	 * Store a list of records. A record that already exists will be updated. A transaction will be used. If there is a problem with storing one 
	 * of the records the whole operation will be rolled back.
	 * 
	 * @param records - the records to store or update
	 * @throws DBException in case of a database problem
	 * @throws NullPointerException when one of the records is null
	 * @throws IllegalArgumentException when one of records cannot be stored
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	public void store(Collection<Record> records) throws DBException, NullPointerException, IllegalArgumentException, IllegalStateException
	{
		if(records == null)
			return;
		int[] results = new int[records.size()]; 
		startTransaction();
		int r = 0;
		try
		{
			for(Record record : records)
			{
				checkStorable(record); // throws NullPointerException, IllegalArgumentException
				results[r++] = doStore(record, ACTION_INSERT_OR_UPDATE);
			}
		}
		catch(Exception e)
		{
			rollbackTransactions();
			throw new DBException(e);
		}
		commitTransaction();
		// Inform client:
		r = 0;
		for(Record record : records)
			report(record, ACTION_INSERT_OR_UPDATE, results[r++]); // (will never throw DBPrimaryKeyException because action is ACTION_INSERT_OR_UPDATE)
	}
	
	/**
	 * @param record
	 * @param storeAction
	 * @param storeResult
	 * @throws DBPrimaryKeyException
	 */
	protected void report(Record record, int storeAction, int storeResult) throws DBPrimaryKeyException
	{
		switch(storeResult)
		{
			case RESULT_NEEDED_INSERT :
				if(storeAction == ACTION_INSERT_OR_UPDATE || storeAction == ACTION_INSERT_ONLY)
					// Record was INSERTed (i.e. it didn't exist before) ...
					client.recordInserted(record); // inform client
				else //if(storeAction == ACTION_UPDATE_ONLY || storeAction == ACTION_UPDATE_ONLY_EXCEPT_LSA)
					// Record did not exist in database (and still doesn't because INSERT was not allowed), so it could not be UPDATEd...
					throw new DBPrimaryKeyException("No such record exists in the record store.");
				break;
			case RESULT_NEEDED_UPDATE :
				if(storeAction != ACTION_INSERT_ONLY)
					// Record existed and was UPDATEd...
					client.recordUpdated(record); // inform client
				else //if(storeAction == ACTION_INSERT_ONLY)
					// Record existed and would have been UPDATEd if it were allowed...
					throw new DBPrimaryKeyException("This record already exists in the record store (with different values).");
				break;
			case RESULT_NEEDED_NO_ACTION :
				// Record was already stored with identical values (except maybe LSA)
				break; // do nothing
		}
	}
	
	/**
	 * Store a record by INSERTing, if it is new (provided INSERTing is allowed), or UPDATEing if it existed (provided UPDATEing is allowed).
	 * 
	 * @param record - the record to store or update; can be assumed to be non-null and not of an internal schema
	 * @param action - an {@code int} representing the allowed storage action(s), possible values: {@link #ACTION_INSERT_OR_UPDATE}, {@link #ACTION_INSERT_ONLY}, {@link #ACTION_UPDATE_ONLY} & {@link #ACTION_UPDATE_ONLY_EXCEPT_LSA}
	 * @return an {@code int} representing the result of the storing operation, possible values: {@link #RESULT_NEEDED_INSERT}, {@link #RESULT_NEEDED_UPDATE} & {@link #RESULT_NEEDED_NO_ACTION}
	 * @throws DBConstraintException when a table/index constraint is violated
	 * @throws DBException in case of a database problem
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	protected abstract int doStore(Record record, int action) throws DBConstraintException, DBException, IllegalStateException;
	
	/**
	 * Retrieve all Records (of any schema)
	 * 
	 * @return a list of records, possibly empty, never null
	 */
	public List<Record> retrieveAllRecords()
	{
		return retrieveRecords(RecordsQuery.ALL);
	}
	
	/**
	 * Retrieve Records of a given Schema
	 * 
	 * @param schema
	 * @return a list of records, possibly empty, never null
	 */
	public List<Record> retrieveRecords(Schema schema)
	{
		return retrieveRecords(new RecordsQuery(Source.From(schema)));
	}
	
	/**
	 * Retrieve Records of given Schemata
	 * 
	 * @param schemata
	 * @return a list of records, possibly empty, never null
	 */
	public List<Record> retrieveRecords(Set<Schema> schemata)
	{
		return retrieveRecords(new RecordsQuery(Source.From(schemata)));
	}

	/**
	 * Retrieve records by query
	 * 
	 * @param query
	 * @return a list of records, possibly empty, never null
	 */
	public abstract List<Record> retrieveRecords(RecordsQuery query);
	
	/**
	 * Retrieve a single record by SingleRecordQuery.
	 * 
	 * @param query
	 * @return the resulting record or {@code null} if no matching record was found
	 */
	public abstract Record retrieveRecord(SingleRecordQuery query);

	/**
	 * Deletes a single record.
	 * Note that this method does not start a new transaction. If this is a desired the client code should take care of that by first calling {@link #startTransaction()}.
	 * However, if an error occurs any open transaction will be rolled back!
	 * 
	 * @param record - the record to delete
	 * @throws DBException
	 */
	public void delete(Record record) throws DBException
	{
		try
		{
			doDelete(record);
		}
		catch(DBException e)
		{
			rollbackTransactions(); // !!!
			throw e;
		}
		// Inform client:
		client.recordDeleted(record);
	}
	
	/**
	 * Deletes the record pointed to by the given reference.
	 * 
	 * Default implementation, may be overridden.
	 * 
	 * @param recordRef
	 * @throws DBException
	 * @throws IllegalStateException when not all columns of this recordReference have been assigned a value
	 */
	public void delete(RecordReference recordRef) throws DBException, IllegalStateException
	{
		delete(recordRef.getRecordQuery().getRecordsQuery());
	}
	
	/**
	 * Deletes all records that match the query
	 * 
	 * Default implementation, may be overridden.
	 * 
	 * @param recordsQuery
	 * @throws DBException
	 */
	public void delete(RecordsQuery query) throws DBException
	{
		delete(retrieveRecords(query));
	}
	
	/**
	 * Deletes a series of records.
	 * A transaction will be used. Upon an error the whole operation will be rolled back.
	 * 
	 * @param records - the records to delete
	 * @throws DBException
	 */
	public void delete(Collection<Record> records) throws DBException
	{
		startTransaction();
		try
		{
			for(Record record : records)
				doDelete(record);
		}
		catch(DBException e)
		{
			rollbackTransactions();
			throw e;
		}
		commitTransaction();
		// Inform client:
		for(Record record : records)
			client.recordDeleted(record);
	}
	
	/**
	 * Deletes *ALL* records.
	 * USE WITH CARE!
	 * 
	 * A transaction will be used. Upon an error the whole operation will be rolled back.
	 * 
	 * @throws DBException
	 */
	public void deleteAllRecords() throws DBException
	{
		delete(retrieveAllDeletableRecords());
	}
	
	/**
	 * Meant to be overridden in cases where the database contains more deletable
	 * record instances than those returned by {@link #retrieveAllRecords()}.
	 * 
	 * @return list of deletable records, possibly empty, never null
	 */
	protected List<Record> retrieveAllDeletableRecords()
	{
		return retrieveAllRecords();
	}
	
	/**
	 * @param record - the record to delete
	 * @throws DBException
	 */
	protected abstract void doDelete(Record record) throws DBException;
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#finalise()
	 */
	public void finalise() throws DBException
	{
		if(isInTransaction())
			System.err.println("Warning: record store is being closed but there is an uncommited transaction (changes may be lost)!");
		// Clean-up:
		cleanup();
		// Close:
		close();
	}
	
	/**
	 * May be overridden
	 */
	protected void cleanup() throws DBException
	{
		// does nothing by default
	}
	
	protected abstract void close() throws DBException;
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#backup(uk.ac.ucl.excites.sapelli.shared.db.StoreBackuper, java.io.File)
	 */
	@Override
	public void backup(StoreBackuper backuper, File destinationFolder) throws DBException
	{
		if(isInTransaction())
			throw new DBException("Cannot back-up database due to uncommited transaction!");
		doBackup(backuper, destinationFolder);
	}
	
	/**
	 * @param backuper
	 * @param destinationFolder
	 * @throws DBException
	 */
	protected abstract void doBackup(StoreBackuper backuper, File destinationFolder) throws DBException;
	
	/**
	 * @return whether or not this RecordStore implementation has full support for indexes (and the constraints they impose)
	 */
	public abstract boolean hasFullIndexSupport();
	
	/**
	 * A task to execute upon roll-back of open transaction(s)
	 * 
	 * @author mstevens
	 */
	protected interface RollbackTask
	{
		
		public void run() throws DBException;
		
	}

}
