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
import java.util.Collection;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
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

	static public final String DATABASE_NAME_SUFFIX = "-RecordStore";
	static public final String BACKUP_SUFFIX = "_Backup";
	
	protected StorageClient client;
	
	/**
	 * Few DBMSs support nested transactions, but this counter allows us to simulate them,
	 * which enables us to keep code that deals with transactions simpler. 
	 */
	private int openTransactions = 0;
	
	public RecordStore(StorageClient client)
	{
		this.client = client;
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
	}
	
	protected abstract void doStartTransaction() throws DBException;
	
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
		}
		else
			System.err.println("Warning: there is no open transaction to commit!");
	}
	
	protected abstract void doCommitTransaction() throws DBException;
	
	/**
	 * Rolls back all(!) transactions.
	 * 
	 * @throws DBException
	 */
	public void rollbackTransactions() throws DBException
	{
		if(openTransactions == 0)
			System.err.println("Warning: there is no open transaction to roll back!");
		while(openTransactions > 0)
			rollbackTransaction();
	}
	
	/**
	 * Rolls back the current transaction.
	 * 
	 * @throws DBException
	 */
	public void rollbackTransaction() throws DBException
	{
		if(openTransactions > 0)
		{
			try
			{
				doRollbackTransaction();
			}
			catch(DBException dbE)
			{
				throw dbE;
			}
			openTransactions--; // !!!
		}
		else
			System.err.println("Warning: there is no open transaction to roll back!");
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
	protected int getOpenTransactions()
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
	 * Stores a single record.
	 * Note that this method does not start a new transaction. If this is a desired the client code should take care of that by first calling {@link #startTransaction()}.
	 * However, if an error occurs any open transaction will be rolled back!
	 * 
	 * @param record - the record to store or update; records of internal schemata will be rejected
	 * @throws DBException
	 */
	public void store(Record record) throws DBException
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
			e.printStackTrace(System.err);
			rollbackTransactions(); // !!!
			throw e;
		}
		// Inform client:
		if(insert)
			client.recordInserted(record);
		else
			client.recordUpdated(record);
	}
	
	/**
	 * Store a list of records. A transaction will be used. If there is a problem with storing one 
	 * of the records the whole operation will be rolled back.
	 * 
	 * @param records - the records to store or update
	 * @throws DBException
	 */
	public void store(List<Record> records) throws DBException
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
			if(insert[r++])
				client.recordInserted(record);
			else
				client.recordUpdated(record);
	}
	
	/**
	 * @param record - the record to store or update; can be assumed to be non-null and not of an internal schema
	 * @throws DBException
	 * @return whether the record was new (i.e. it was INSERTed; returns true), or not (i.e. it was UPDATEd; returns false)
	 */
	protected abstract boolean doStore(Record record) throws DBException;
	
	/**
	 * Retrieve all Records (of any schema)
	 * 
	 * @return
	 */
	public List<Record> retrieveAllRecords()
	{
		return retrieveRecords(RecordsQuery.ALL);
	}
	
	/**
	 * Retrieve Records of a given Schema
	 * 
	 * @param schema
	 * @return
	 */
	public List<Record> retrieveRecords(Schema schema)
	{
		return retrieveRecords(new RecordsQuery(Source.From(schema)));
	}
	
	/**
	 * Retrieve Records of given Schemata
	 * 
	 * @param schemata
	 * @return
	 */
	public List<Record> retrieveRecords(Set<Schema> schemata)
	{
		return retrieveRecords(new RecordsQuery(Source.From(schemata)));
	}

	/**
	 * Retrieve records by query
	 * 
	 * @param query
	 * @return
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
			e.printStackTrace(System.err);
			rollbackTransactions(); // !!!
			throw e;
		}
		// Inform client:
		client.recordDeleted(record);
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
			e.printStackTrace(System.err);
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
	 * @return list of deletable records
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
	
	public void backup(File destinationFolder) throws DBException
	{
		if(isInTransaction())
			throw new DBException("Cannot back-up database due to uncommited transaction!"); // TODO remove this?
		doBackup(destinationFolder);
	}
	
	protected abstract void doBackup(File destinationFolder) throws DBException;

}
