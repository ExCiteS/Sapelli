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
import uk.ac.ucl.excites.sapelli.shared.db.StoreBackupper;
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
public abstract class RecordStore extends Store
{

	// STATIC -----------------------------------------------------------------
	static public final String DATABASE_NAME_SUFFIX = "-RecordStore";
	static public final String BACKUP_SUFFIX = "_Backup_"; // to be followed by a timestamp
	
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
	 * @return whether of not the given record can be stored in this RecordStore 
	 */
	public boolean isStorable(Record record)
	{
		return 	record != null &&					// obviously it makes no sense to store null records
				!record.getSchema().isInternal();	// records of "internal" schemata cannot be stored directly
	}
	
	/**
	 * Stores a single record, if it already exists it is updated.
	 * Note that this method does not start a new transaction. If this is a desired the client code should take care of that by first calling {@link #startTransaction()}.
	 * However, if an error occurs any open transaction will be rolled back!
	 * 
	 * @param record - the record to store or update; records of internal schemata will be rejected
	 * @throws DBConstraintException when a table/index constraint is violated
	 * @throws DBException in case of a database problem
	 * @throws IllegalArgumentException when the given record cannot be stored
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	public void store(Record record) throws DBException, IllegalArgumentException, IllegalStateException
	{
		if(!isStorable(record))
			throw new IllegalArgumentException(String.format("Record (%s) cannot be stored!", record.toString(false)));
		Boolean insert = null;
		try
		{
			insert = doStore(record);
		}
		catch(DBException e)
		{
			rollbackTransactions(); // !!!
			throw e;
		}
		// Inform client:
		if(insert == null)
			return; // record was unchanged
		else if(insert)
			client.recordInserted(record);
		else
			client.recordUpdated(record);
	}
	
	/**
	 * Insert a single record, if it already exists a DuplicateException will be thrown.
	 * Note that this method does not start a new transaction. If this is a desired the client code should take care of that by first calling {@link #startTransaction()}.
	 * However, if an error occurs any open transaction will be rolled back!
	 * 
	 * @param record
	 * @throws DBPrimaryKeyException when the record already exists
	 * @throws DBConstraintException when a table/index constraint is violated
	 * @throws DBException in case of another database problem
	 * @throws IllegalArgumentException when the given record cannot be stored
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	public void insert(Record record) throws DBPrimaryKeyException, DBConstraintException, DBException, IllegalArgumentException, IllegalStateException
	{
		if(!isStorable(record))
			throw new IllegalArgumentException(String.format("Record (%s) cannot be inserted!", record.toString(false)));
		boolean inserted = false;
		try
		{
			inserted = doInsert(record);
		}
		catch(DBException e)
		{
			rollbackTransactions(); // !!!
			throw e;
		}
		// Inform client if a real insert happened:
		if(inserted)
			client.recordInserted(record);
	}
	
	/**
	 * Store a list of records. A record that already exists will be updated. A transaction will be used. If there is a problem with storing one 
	 * of the records the whole operation will be rolled back.
	 * 
	 * @param records - the records to store or update
	 * @throws DBException in case of a database problem
	 * @throws IllegalArgumentException when the given record cannot be stored
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	public void store(List<Record> records) throws DBException, IllegalArgumentException, IllegalStateException
	{
		Boolean[] insert = new Boolean[records.size()]; 
		startTransaction();
		int r = 0;
		try
		{
			for(Record record : records)
				if(isStorable(record))
					insert[r++] = doStore(record);
				else
					throw new IllegalArgumentException(String.format("Record (%s) cannot be stored!", record.toString(false)));
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
		{
			if(insert[r] == null)
				return; // record was unchanged
			else if(insert[r])
				client.recordInserted(record);
			else
				client.recordUpdated(record);
			r++;
		}
	}
	
	/**
	 * Stores (insert or update/replace) a record
	 * 
	 * @param record - the record to store or update; can be assumed to be non-null and not of an internal schema
	 * @return whether the record was new (i.e. it was INSERTed; returns {@code true}), modified (i.e. it was UPDATEd; returns {@code false}), or neither (i.e. the exact same record was already stored; returns {@code null})
	 * @throws DBConstraintException when a table/index constraint is violated
	 * @throws DBException in case of a database problem
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	protected abstract Boolean doStore(Record record) throws DBConstraintException, DBException, IllegalStateException;
	
	/**
	 * Inserts a record, throws a DuplicateException if it already exists.
	 * 
	 * @param record - the record to insert; can be assumed to be non-null and not of an internal schema
	 * @return whether the record was really inserted (true), or was already stored with identical values (false)
	 * @throws DBPrimaryKeyException when the record already exists
	 * @throws DBConstraintException when a table/index constraint is violated
	 * @throws DBException in case of another database problem
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	protected abstract boolean doInsert(Record record) throws DBPrimaryKeyException, DBConstraintException, DBException, IllegalStateException;
	
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
	 * Retrieve a single record pointed to by the given RecordReference.
	 * 
	 * @param recordReference
	 * @return the resulting record or {@code null} if no matching record was found
	 */
	public Record retrieveRecord(RecordReference recordReference)
	{
		return retrieveRecord(recordReference.getRecordQuery());
	}
	
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
		List<Record> deleted = new ArrayList<Record>(records.size());
		try
		{
			for(Record record : records)
				if(doDelete(record))
					deleted.add(record);
		}
		catch(DBException e)
		{
			rollbackTransactions();
			throw e;
		}
		commitTransaction();
		// Inform client:
		for(Record record : deleted)
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
	 * @return whether or not the record was really deleted
	 * @throws DBException
	 */
	protected abstract boolean doDelete(Record record) throws DBException;
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#finalise()
	 */
	protected void doClose() throws DBException
	{
		if(isInTransaction())
			System.err.println("Warning: record store is being closed but there is an uncommited transaction (changes may be lost)!");
		// Clean-up:
		cleanup();
		// Close DB connection:
		closeConnection();
	}
	
	/**
	 * May be overridden
	 */
	protected void cleanup() throws DBException
	{
		// does nothing by default
	}
	
	/**
	 * Subclasses must implement this to close the connection to the underlying database file/service
	 * 
	 * @throws DBException
	 */
	protected abstract void closeConnection() throws DBException;
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#backup(uk.ac.ucl.excites.sapelli.shared.db.StoreBackuper, java.io.File)
	 */
	@Override
	public void backup(StoreBackupper backuper, File destinationFolder) throws DBException
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
	protected abstract void doBackup(StoreBackupper backuper, File destinationFolder) throws DBException;
	
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
