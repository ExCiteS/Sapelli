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
	
	protected abstract void startTransaction();
	
	protected abstract void commitTransaction();
	
	protected abstract void rollbackTransaction();

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
		if(isStorable(record))
		{
			startTransaction();
			try
			{
				doStore(record);
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				rollbackTransaction();
				throw e;
			}
			commitTransaction();
		}
	}
	
	/**
	 * @param records - the records to store or update
	 * @throws Exception
	 */
	public void store(List<Record> records) throws Exception
	{
		startTransaction();
		try
		{
			for(Record r : records)
				if(isStorable(r))
					doStore(r);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			rollbackTransaction();
			throw e;
		}
		commitTransaction();
	}
	
	/**
	 * @param record - the record to store or update; can be assumed to be non-null and not of an internal schema
	 * @throws Exception
	 */
	protected abstract void doStore(Record record) throws Exception;
	
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
	 * Retrieve a single record by SingleRecordQuery 
	 * 
	 * @param query
	 * @return
	 */
	public abstract Record retrieveRecord(SingleRecordQuery query);

	/**
	 * @param record - the record to delete
	 * @throws Exception
	 */
	public void delete(Record record) throws Exception
	{
		startTransaction();
		try
		{
			doDelete(record);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			rollbackTransaction();
			throw e;
		}
		commitTransaction();
	}

	/**
	 * @param records - the records to delete
	 * @throws Exception
	 */
	public void delete(List<Record> records) throws Exception
	{
		startTransaction();
		try
		{
			for(Record r : records)
				doDelete(r);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			rollbackTransaction();
			throw e;
		}
		commitTransaction();
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
