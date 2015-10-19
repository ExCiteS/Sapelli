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

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.shared.db.StoreBackupper;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.SQLTable;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.TableFactory;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;

/**
 * @author mstevens
 *
 */
public abstract class SQLRecordStoreUpgrader
{
	
	private final List<String> warnings;
	protected final UpgradeCallback callback;
	protected final File backupFolder;
	private final Map<Integer, UpgradeStep<?>> steps;
	
	@SafeVarargs
	public SQLRecordStoreUpgrader(UpgradeCallback callback, File backupFolder, UpgradeStep<?>... steps)
	{
		this.warnings = new ArrayList<String>();
		this.callback = callback;
		this.backupFolder = backupFolder;
		this.steps = new HashMap<Integer, UpgradeStep<?>>(steps != null ? steps.length : 0);
		if(steps != null)
			for(UpgradeStep<?> step : steps)
				this.steps.put(step.fromVersion, step);
	}
	
	/**
	 * To be called from {@link RecordStore#initialise(boolean, int, Upgrader)}.
	 *  
	 * @param recordStore
	 * @param toVersion
	 * @throws DBException
	 */
	public final void upgrade(SQLRecordStore<?, ?, ?> recordStore, final int toVersion) throws DBException
	{
		final int fromVersion = recordStore.getVersion();
		
		// Clear previous warnings:
		warnings.clear();
		
		// Enable logging:
		boolean wasLoggingEnabled = recordStore.isLoggingEnabled();
		recordStore.setLoggingEnabled(true);
		
		// Apply step-by-step upgrade:
		int currentVersion;
		while((currentVersion = recordStore.getVersion()) < toVersion)
		{
			// Get the right upgrade step:
			UpgradeStep<?> step = steps.get(currentVersion);
			if(step == null)
				throw new DBException("No UpgradeStep for current version (" + currentVersion + ") found!");
			
			// Backup first:
			try
			{
				StoreBackupper.Backup(FileHelpers.getSubDirectory(backupFolder, "v" + currentVersion, true), true, recordStore);
			}
			catch(Exception e)
			{
				throw new DBException("Database backup prior to upgrade from v" + currentVersion + " to v" + step.toVersion + " failed!", e);
			}
			
			// Apply upgrade step (always in a transaction):
			try
			{
				// Open transaction:
				recordStore.startTransaction();
				// Apply step:
				step.apply(recordStore, new UpgradeOperations());
				// Set new version:
				recordStore.setVersion(step.toVersion);
				// Close transaction:
				recordStore.commitTransaction();
			}
			catch(Exception e)
			{
				// don't roll back here, SQLRecordStore#initialise() will do that
				throw new DBException("Failed to upgrade database from v" + currentVersion + " to v" + step.toVersion + "!", e);
			}
		}
		
		// Re-disable logging if needed:
		recordStore.setLoggingEnabled(wasLoggingEnabled);
		
		// If successful:
		if(callback != null)
			callback.upgradePerformed(fromVersion, toVersion, warnings);
	}
	
	/**
	 * @author mstevens
	 */
	static public abstract class UpgradeStep<C extends StorageClient>
	{
		
		protected final C client;
		public final int fromVersion;
		public final int toVersion;
		
		public UpgradeStep(C client, int fromVersion)
		{
			this(client, fromVersion, fromVersion + 1);
		}
		
		public UpgradeStep(C client, int fromVersion, int toVersion)
		{
			this.client = client;
			this.fromVersion = fromVersion;
			this.toVersion = toVersion;
		}
		
		public abstract void apply(SQLRecordStore<?, ?, ?> recordStore, UpgradeOperations upgradeOperations) throws Exception;
		
	}
	
	/**
	 * Helper class which grants UpgradeSteps access to a number of {@link SQLRecordStore} methods would otherwise be
	 * inaccessible due to their package or protected access level.
	 * 
	 * @author mstevens
	 */
	public class UpgradeOperations
	{
		
		/**
		 * @see SQLRecordStore#doesTableExist(String)
		 */
		public boolean doesTableExist(SQLRecordStore<?, ?, ?> recordStore, String tableName)
		{
			return recordStore.doesTableExist(tableName) || recordStore.doesTableExist(recordStore.sanitiseIdentifier(tableName));
		}
		
		/**
		 * @see SQLRecordStore#sanitiseIdentifier(String)
		 */
		public String sanitiseIdentifier(SQLRecordStore<?, ?, ?> recordStore, String identifier)
		{
			return recordStore.sanitiseIdentifier(identifier);
		}
		
		/**
		 * @see SQLRecordStore#dropTable(String, boolean)
		 */
		public void dropTable(SQLRecordStore<?, ?, ?> recordStore, String tableName, boolean force) throws DBException
		{
			recordStore.dropTable(tableName, force);
		}
		
		/**
		 * @see SQLRecordStore#renameTable(String, String)
		 */
		public void renameTable(SQLRecordStore<?, ?, ?> recordStore, String oldTableName, String newTableName) throws DBException
		{
			recordStore.renameTable(oldTableName, newTableName);
		}
		
		/**
		 * @see SQLRecordStore#getAllTableNames()
		 */
		public List<String> getAllTableNames(SQLRecordStore<?, ?, ?> recordStore)
		{
			return recordStore.getAllTableNames();
		}
		
		/**
		 * @see SQLRecordStore#getAllKnownSchemata()
		 */
		public List<Schema> getAllSchemata(SQLRecordStore<?, ?, ?> recordStore)
		{
			return recordStore.getAllKnownSchemata();
		}
		
		/**
		 * @see SQLRecordStore#getTableFactory()
		 */
		public TableFactory<?> getTableFactory(SQLRecordStore<?, ?, ?> recordStore)
		{
			return recordStore.getTableFactory();
		}
		
		/**
		 * @see SQLRecordStore#cleanup()
		 */
		public void cleanup(SQLRecordStore<?, ?, ?> recordStore) throws DBException
		{
			recordStore.cleanup();
		}
		
		/**
		 * @see SQLRecordStore#release()
		 */
		public void release(SQLRecordStore<?, ?, ?> recordStore)
		{
			recordStore.release();
		}
		
		public void addWarning(String warning)
		{
			warnings.add(warning);
		}

		public void addWarnings(Collection<String> warnings)
		{
			SQLRecordStoreUpgrader.this.warnings.addAll(warnings);
		}
		
		/**
		 * TODO needs testing, not sure I ever got this to really work
		 * 
		 * @param recordStore
		 * @param newSchema must already contain the new Columns
		 * @param replacers
		 * @return the converted records, which are yet to be inserted!
		 * @throws DBException
		 */
		@SuppressWarnings({ "unchecked", "rawtypes", "deprecation" })
		public List<Record> replace(SQLRecordStore<?, ?, ?> recordStore, Schema newSchema, List<ColumnReplacer<?, ?>> replacers) throws DBException
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
		
	}
	
	/**
	 * @author mstevens
	 */
	public interface UpgradeCallback
	{
		
		public void upgradePerformed(int fromVersion, int toVersion, List<String> warnings);
		
	}
	
	/**
	 * TODO needs testing, not sure I ever got this to really work
	 * 
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
	 * TODO needs testing, not sure I ever got this to really work
	 * 
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
