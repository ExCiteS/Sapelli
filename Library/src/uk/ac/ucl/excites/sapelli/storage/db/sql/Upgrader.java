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

package uk.ac.ucl.excites.sapelli.storage.db.sql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.SQLTable;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;

public abstract class Upgrader
{
	
	/**
	 * To be called from {@link SQLRecordStore#initialise(boolean, int, Upgrader)}.
	 *  
	 * @param recordStore
	 * @param oldVersion
	 * @param newVersion
	 * @throws DBException
	 */
	public abstract void upgrade(SQLRecordStore<?, ?, ?> recordStore, int oldVersion, int newVersion) throws DBException;
	
	protected List<Schema> getAllSchemata(SQLRecordStore<?, ?, ?> recordStore)
	{
		return recordStore.getAllKnownSchemata();
	}
	
	protected void cleanup(SQLRecordStore<?, ?, ?> recordStore) throws DBException
	{
		recordStore.cleanup();
	}
	
	/**
	 * @param newSchema must already contain the newColumn
	 * @param oldColumn must be top-level
	 * @param newColumn must be top-level
	 * @param converter
	 * @return the converted records, which are yet to be inserted!
	 * @throws DBException
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected <OT, NT> List<Record> replace(SQLRecordStore<?, ?, ?> recordStore, Schema newSchema, Column<OT> oldColumn, Column<NT> newColumn, ValueConverter<OT, NT> converter) throws DBException
	{
		if(!recordStore.doesTableExist(newSchema)) // only based on schema name (no STable object is instantiated)
			return Collections.<Record> emptyList();
		
		// get Schema object as currently stored
		Schema oldSchema = recordStore.getStoredVersion(newSchema);
		if(oldSchema == null || !oldSchema.containsEquivalentColumn(oldColumn))
			throw new DBException("TODO");
		// get STable for oldSchema:
		SQLTable oldTable = recordStore.getTableFactory().generateTable(oldSchema);
		// get STable for newSchema:
		//STable newTable = getTableFactory().generateTable(newSchema);
		
		// Retrieve all records currently in the (old) table:
		List<Record> oldRecords = oldTable.select(RecordsQuery.ALL);
		
		// Drop old table:
		oldTable.drop();
		oldTable.release();
		
		// Create new table:
		//newTable.create();
		
		// Convert & store records:
		List<Record> newRecords = new ArrayList<Record>(oldRecords.size());
		for(Record oldRec : oldRecords)
		{
			Record newRec = newSchema.createRecord();
			for(Column<?> oldCol : oldSchema.getColumns(false))
				if(oldCol.equals(oldColumn))
					newColumn.storeValue(newRec, converter.convert(oldColumn.retrieveValue(oldRec)));
				else
					newSchema.getEquivalentColumn(oldCol).storeObject(newRec, oldCol.retrieveValue(oldRec));
			//newTable.insert(newRec);
			newRecords.add(newRec);
		}
		
		return newRecords;
	}
	
	protected interface ValueConverter<OT, NT>
	{
		
		public NT convert(OT originalValue);
		
	}

}
