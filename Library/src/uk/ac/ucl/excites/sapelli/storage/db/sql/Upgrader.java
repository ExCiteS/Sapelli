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

/**
 * @author mstevens
 *
 */
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
	 * @param recordStore
	 * @param newSchema must already contain the new Columns
	 * @param replacers
	 * @return the converted records, which are yet to be inserted!
	 * @throws DBException
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected List<Record> replace(SQLRecordStore<?, ?, ?> recordStore, Schema newSchema, List<ColumnReplacer<?, ?>> replacers) throws DBException
	{
		if(!recordStore.doesTableExist(newSchema)) // only based on schema name (no STable object is instantiated)
			return Collections.<Record> emptyList();
		
		// get Schema object as currently stored
		Schema oldSchema = recordStore.getStoredVersion(newSchema);
		if(oldSchema == null /*|| !oldSchema.containsEquivalentColumn(oldColumn)*/)
			throw new DBException("TODO"); // TODO
		
		// get STable for oldSchema:
		SQLTable oldTable = recordStore.getTableFactory().generateTable(oldSchema);
		// get STable for newSchema:
		//STable newTable = getTableFactory().generateTable(newSchema);
		
		// Retrieve all records currently in the (old) table:
		List<Record> oldRecords = oldTable.select(new RecordsQuery(oldTable.schema));
		
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
			cols : for(Column<?> oldCol : oldSchema.getColumns(false))
			{
				for(ColumnReplacer<?, ?> replacer : replacers) // loop over replacers to see if one of them deals with the current oldCol
					if(replacer.replace(oldCol, newRec, oldRec))
						continue cols; // value was converted or skipped
				//else (oldCol is not replaced or deleted):
				newSchema.getEquivalentColumn(oldCol).storeObject(newRec, oldCol.retrieveValue(oldRec));
			}
			//newTable.insert(newRec);
			newRecords.add(newRec);
		}
		
		return newRecords;
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <OT>
	 * @param <NT>
	 */
	public abstract class ColumnReplacer<OT, NT>
	{
		
		final Column<OT> oldColumn;
		
		final Column<NT> newColumn;
		
		/**
		 * @param oldColumn must be top-level
		 * @param newColumn must be top-level, may be null if oldColumn is being deleted instead of replaced
		 */
		public ColumnReplacer(Column<OT> oldColumn, Column<NT> newColumn)
		{
			this.oldColumn = oldColumn;
			this.newColumn = newColumn;
		}
		
		public boolean replace(Column<?> oldCol, Record newRec, Record oldRec)
		{
			if(oldCol.equals(oldColumn))
			{
				if(newColumn != null) // if newColumn == null the oldColumn has been deleted without a replacement
					newColumn.storeValue(newRec, convert(oldColumn.retrieveValue(oldRec)));
				return true; // value was replaces (or skipped)
			}
			return false;
		}
		
		public abstract NT convert(OT originalValue);
		
	}

	/**
	 * @author mstevens
	 *
	 * @param <OT>
	 */
	public abstract class ColumnDeleter<OT> extends ColumnReplacer<OT, Void>
	{

		public ColumnDeleter(Column<OT> oldColumn)
		{
			super(oldColumn, null);
		}
		
	}
	
}
