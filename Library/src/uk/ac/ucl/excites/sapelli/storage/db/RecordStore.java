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
	
	public abstract void startTransaction();
	
	public abstract void commitTransaction();
	
	public abstract void rollbackTransaction();

	public boolean isStorable(Record record)
	{
		return 	record != null &&					// obviously it makes no sense to store null records
				!record.getSchema().isInternal();	// records of "internal" schemata cannot be stored directly
	}
	
	/**
	 * @param record - the record to store or update; records of internal schemata will be rejected
	 * 
	 * @throws Exception
	 */
	public void store(Record record) throws Exception
	{
		store(record, true);
	}
	
	/**
	 * @param record - the record to store or update; records of internal schemata will be rejected
	 * @param useTransaction - whether or not to start & commit a transaction
	 * @throws Exception
	 */
	public void store(Record record, boolean useTransaction) throws Exception
	{
		if(!isStorable(record))
			throw new IllegalArgumentException(String.format("Record (%s) cannot be stored!", record.toString(false)));
		Boolean insert = null;
		if(useTransaction)
			startTransaction();
		try
		{
			insert = doStore(record);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			if(useTransaction)
				rollbackTransaction();
			throw e;
		}
		if(useTransaction)
			commitTransaction();
		// Inform client:
		if(insert)
			client.recordInserted(record);
		else
			client.recordUpdated(record);
	}
	
	/**
	 * @param records - the records to store or update
	 * @throws Exception
	 */
	public void store(List<Record> records) throws Exception
	{
		store(records, true);
	}
	
	/**
	 * @param records - the records to store or update
	 * @param useTransaction - whether or not to start & commit a transaction
	 * @throws Exception
	 */
	public void store(List<Record> records, boolean useTransaction) throws Exception
	{
		Boolean[] insert = new Boolean[records.size()]; 
		if(useTransaction)
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
			e.printStackTrace(System.err);
			if(useTransaction)
				rollbackTransaction();
			throw e;
		}
		if(useTransaction)
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
	 * @throws Exception
	 * @return whether the record was new (i.e. it was INSERTed; returns true), or not (i.e. it was UPDATEd; returns false)
	 */
	protected abstract boolean doStore(Record record) throws Exception;
	
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
	 * Retrieve Records of given Schemata
	 * 
	 * @param schemata
	 * @return
	 */
	public List<Record> retrieveRecords(List<Schema> schemata)
	{
		return retrieveRecords(new RecordsQuery(schemata));
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
	 * @param record - the record to delete
	 * @throws Exception
	 */
	public void delete(Record record) throws Exception
	{
		delete(record, true);
	}
	
	/**
	 * @param record - the record to delete
	 * @param useTransaction - whether or not to start & commit a transaction
	 * @throws Exception
	 */
	public void delete(Record record, boolean useTransaction) throws Exception
	{
		if(useTransaction)
			startTransaction();
		try
		{
			doDelete(record);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			if(useTransaction)
				rollbackTransaction();
			throw e;
		}
		if(useTransaction)
			commitTransaction();
		// Inform client:
		client.recordDeleted(record);
	}

	/**
	 * @param records - the records to delete
	 * @throws Exception
	 */
	public void delete(List<Record> records) throws Exception
	{
		delete(records, true);
	}
	
	/**
	 * @param records - the records to delete
	 * @param useTransaction - whether or not to start & commit a transaction
	 * @throws Exception
	 */
	public void delete(List<Record> records, boolean useTransaction) throws Exception
	{
		if(useTransaction)
			startTransaction();
		try
		{
			for(Record record : records)
				doDelete(record);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			if(useTransaction)
				rollbackTransaction();
			throw e;
		}
		if(useTransaction)
			commitTransaction();
		// Inform client:
		for(Record record : records)
			client.recordDeleted(record);
	}
	
	/**
	 * Deletes *ALL* records.
	 * USE WITH CARE!
	 * 
	 * @throws Exception
	 */
	public void deleteAllRecords() throws Exception
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
	 * @throws Exception
	 */
	protected abstract void doDelete(Record record) throws Exception;

}
