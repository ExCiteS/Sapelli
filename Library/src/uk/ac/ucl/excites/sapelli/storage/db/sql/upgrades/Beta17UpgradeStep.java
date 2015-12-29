/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.storage.db.sql.upgrades;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader.DefaultValueColumnAdder;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader.TableConverter;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader.UpgradeOperations;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader.UpgradeStep;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteBooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LosslessFlagColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;

/**
 * UpgradeStep class which upgrades SQLRecordStore databases from Sapelli version prior to v2.0 Beta 17.
 * It reflects a number of important changes that have been made in the Sapelli Storage layer:
 * 	- new way of serialising Model objects in {@link Model#MODEL_SCHEMA} records;
 *  - the renaming of the {@link Model#MODEL_SERIALISATION_COLUMN};
 *  - the Schema {@code {@link Schema#flags} mechanism
 *  - the new {@link Model#SCHEMA_NAME_COLUMN};
 *  - the renaming of existing tables according to the new names of certain internal tables and the way of generating and storing table names in general;
 *  - the name {@link Model#SCHEMA_TABLE_NAME_COLUMN}
 *  - the new use, in {@link SQLiteRecordStore}, of a top-level {@link SQLiteBooleanColumn} to represent {@link ValueSetColumn}s with all-optional subcolumns, in order to maintain the difference between a null ValueSet and an empty one.  
 * 
 * Note: This upgrade only works if the tableName for ProjectRecordStore#PROJECT_SCHEMA does not change (i.e. remains "Collector_Projects")
 * 
 * @author mstevens
 */
public abstract class Beta17UpgradeStep<C extends StorageClient> extends UpgradeStep<C>
{
	
	/**
	 * @param client
	 * @param fromVersion
	 * @param toVersion
	 */
	public Beta17UpgradeStep(C client, int fromVersion, int toVersion)
	{
		super(client, fromVersion, toVersion);
	}

	/**
	 * @param client
	 * @param fromVersion
	 */
	public Beta17UpgradeStep(C client, int fromVersion)
	{
		super(client, fromVersion);
	}

	@Override
	public void apply(SQLRecordStore<?, ?, ?> recordStore, UpgradeOperations upgradeOps) throws Exception
	{		
		// Retrieve all known Models as RecordReferences:
		List<RecordReference> modelRecRefs = recordStore.retrieveRecordReferences(new RecordsQuery(Model.MODEL_SCHEMA));
		/* The RecordReferences contain enough information (all we really need is the modelID) and retrieving them does
		 * not involve Model deserialisation (which would fail because the currently stored compressed serialised Java
		 * objects are no longer compatible with the current Model, Schema, Column, etc. classes) */
		
		List<Record> newModelRecs = new ArrayList<Record>();
		List<Schema> schemata = new ArrayList<Schema>();
		
		// Loop over the Model recordReferences:
		for(RecordReference modelRecRef : modelRecRefs)
			try
			{
				Model model = client.getModel(Model.MODEL_ID_COLUMN.retrieveValue(modelRecRef)); // this will involve querying the ProjectRecordStore
				
				// Create new Model record (to be stored in Models table below):
				newModelRecs.add(model.getModelRecord(client));
				
				// Remember schema for use below:
				schemata.addAll(model.getSchemata());
			}
			catch(UnknownModelException ume)
			{
				ume.printStackTrace();
				upgradeOps.addWarning("Failed to get Model instance for modelID " + Model.MODEL_ID_COLUMN.retrieveValue(modelRecRef) + ", corresponding tables will be deleted.");
			}
		
		// Drop existing Models & Schemata tables (will be recreated below):
		upgradeOps.dropTable(recordStore, getOldTableName(Model.MODEL_SCHEMA), true);
		upgradeOps.dropTable(recordStore, getOldTableName(Model.SCHEMA_SCHEMA), true);
		
		// Create new Models table and insert new records:
		recordStore.store(newModelRecs); // this also achieves renaming the "compressedSerialisedObject" column to "serialisation"
		
		// List for the names of all tables that should be kept:
		Set<String> keepTables = new HashSet<String>(recordStore.getProtectedTableNames());
		
		// Loop over all schemata:
		for(Schema schema : schemata)
		{
			// Check if there is a table, with the old name (which is not necessarily different from the new name), for the schema:
			if(!upgradeOps.doesTableExist(recordStore, getOldTableName(schema)))
				continue; // if there is no table we are done with this Schema
			
			// Remember (new) table so we don't delete the table below:
			keepTables.add(schema.tableName); // !!!
			
			//	Store new schemata record:
			recordStore.store(schema.getMetaRecord()); // this also achieves adding new "flags" and "tableName" columns
			
			//	Rename table if necessary:
			String oldName = getOldTableName(schema);
			if(!oldName.equals(schema.tableName))
				upgradeOps.renameTable(recordStore, oldName, schema.tableName);
			
			// Get a TableConverter for the schema:
			TableConverter tableConverter = new TableConverter(schema, schema.flags & ~StorageClient.SCHEMA_FLAG_TRACK_LOSSLESSNESS); // un-set the lossless flag on the old schema
			// Add a column replacer to to deal with the added LosslessFlagColumn in schemas that have that flag:
			if(schema.hasFlags(StorageClient.SCHEMA_FLAG_TRACK_LOSSLESSNESS))
				tableConverter.addColumnReplacer(new DefaultValueColumnAdder(LosslessFlagColumn.INSTANCE));
			
			// Give subclass the change to further tweak the table converter:
			customiseTableConverter(schema, tableConverter);
			
			// Check if schema has at least one (non-virtual) ValueSetColumn with all-optional subcolumns:
			boolean hasValueSetColWithAllOptionalSubCols = false;
			for(Column<?> col : schema.getColumns(false))
				if(col instanceof ValueSetColumn<?, ?> && ((ValueSetColumn<?, ?>) col).hasAllOptionalSubColumns())
				{
					hasValueSetColWithAllOptionalSubCols = true;
					break;
				}
			
			// Check if we need to do anything:
			if(tableConverter.isTransparent() && !hasValueSetColWithAllOptionalSubCols)
				// this schema/table does not need conversion.
				continue;
			
			if(hasValueSetColWithAllOptionalSubCols)
				// Temporarily disable the use of boolean columns to represent optional ValueSetColumns:
				upgradeOps.getTableFactory(recordStore).setInsertBoolColsForAllOptionalValueSetCols(false);
				/* This is required because the tables currently existing in the db are incompatible with
				 * the SQLRecordStore#SQLTable instance we would get for the schema if we wouldn't disable
				 * this behaviour. Disabling the behaviour ensures we get a SQLTable that is compatible with
				 * the table as it exists in the db, enabling us to ... */
			
			// ... get all current records by querying the db with the oldSchema:
			List<Record> oldRecords = recordStore.retrieveRecords(tableConverter.getOldSchema());

			// Drop table (this will also get rid of the above-mentioned SQLTable instance):
			upgradeOps.dropTable(recordStore, schema.tableName, false);
			
			if(hasValueSetColWithAllOptionalSubCols)
				// Re-enable the use of boolean columns to represent optional ValueSetColumns:
				upgradeOps.getTableFactory(recordStore).setInsertBoolColsForAllOptionalValueSetCols(true);
				
			// Re-insert all converted records in new table (which will have the boolean column representing the ValueSetColumn):
			recordStore.store(tableConverter.convertRecords(oldRecords));
		}
		
		// Delete unknown/unupgradable tables:
		for(String tableName : upgradeOps.getAllTableNames(recordStore))
			if(!keepTables.contains(tableName) && !keepTables.contains(upgradeOps.sanitiseIdentifier(recordStore, tableName)))
			{
				upgradeOps.addWarning("Deleting unknown table \'" + tableName + "\'");
				upgradeOps.dropTable(recordStore, tableName, false);
			}
		
		// Upgrade step done!
	}
	
	protected abstract void customiseTableConverter(Schema newSchema, TableConverter tableConverter) throws DBException;
	
	/**
	 * @param schema
	 * @return the name the table for the given Schema would have had prior to the upgrade
	 */
	protected abstract String getOldTableName(Schema schema);
	
}
