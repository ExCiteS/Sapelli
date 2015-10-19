/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db.sql.upgrades;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
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
 *  - the new use, in {@link SQLiteRecordStore}, of a top-level {@link SQLiteBooleanColumn} to represent optional {@link ValueSetColumn}s, in order to maintain the difference between a null ValueSet and an empty one.  
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
		
		// Loop over all schemata to rename/recreate existing tables, create new Schemata records (and the table that contains them):
		for(Schema schema : schemata)
		{
			// Check if there is a table, with the old name, for the schema:
			String oldName = getOldTableName(schema);
			if(!upgradeOps.doesTableExist(recordStore, oldName))
				continue; // there no table for this schema
			//else...
			//	Store new schemata record:
			recordStore.store(schema.getMetaRecord()); // this also achieves adding new "flags" and "tableName" columns
			//	Rename table if necessary:
			if(!oldName.equals(schema.tableName))
				upgradeOps.renameTable(recordStore, oldName, schema.tableName);
			// Remember (new) table so we don't delete the table below:
			keepTables.add(schema.tableName); // !!!
			
			// Find all optional ValueSetColumns in the schema:
			boolean hasOptionalValueSetCols = false;
			for(Column<?> col : schema.getColumns(false))
				if(col instanceof ValueSetColumn<?, ?> && col.optional)
				{
					hasOptionalValueSetCols = true;
					break;
				}
			// If the schema has such columns then...
			if(hasOptionalValueSetCols)
			{
				// Temporarily disable the use of boolean columns to represent optional ValueSetColumns:
				upgradeOps.getTableFactory(recordStore).setUseBoolColsForOptionalValueSetCols(false);
				/* This is required because the tables currently existing in the db are incompatible with
				 * the SQLRecordStore#SQLTable instance we would get for the schema if we wouldn't disable
				 * this behaviour. Disabling the behaviour ensures we get a SQLTable that is compatible with
				 * the table as it exists in the db, enabling us to ... */
				
				// ... get all current records:
				List<Record> records = recordStore.retrieveRecords(schema);

				// Drop table (this will also get rid of the above-mentioned SQLTable instance):
				upgradeOps.dropTable(recordStore, schema.tableName, false);
				
				// Re-enable the use of boolean columns to represent optional ValueSetColumns:
				upgradeOps.getTableFactory(recordStore).setUseBoolColsForOptionalValueSetCols(true);
				
				// Re-insert all records in new table (which will have the boolean column representing the ValueSetColumn):
				recordStore.store(records);
			}
		}
		
		// Delete unknown/unupgradable tables:
		for(String tableName : upgradeOps.getAllTableNames(recordStore))
			if(!keepTables.contains(tableName) && !keepTables.contains(upgradeOps.sanitiseIdentifier(recordStore, tableName)))
			{
				upgradeOps.addWarning("Deleting unknown/unupgradable table \'" + tableName + "\'");
				upgradeOps.dropTable(recordStore, tableName, false);
			}
		
		// Upgrade step done!
	}
	
	/**
	 * @param schema
	 * @return the name the table for the given Schema would have had prior to the upgrade
	 */
	protected abstract String getOldTableName(Schema schema);
	
}
