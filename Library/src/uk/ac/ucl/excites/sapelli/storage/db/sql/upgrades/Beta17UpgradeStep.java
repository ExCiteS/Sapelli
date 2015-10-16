/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db.sql.upgrades;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader.UpgradeOperations;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader.UpgradeStep;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;

/**
 * TODO
 * 
 * TODO better class name?
 * 
 * The RecordReferences contain enough information (all we really need is the modelID) and 
 * retrieving them does not involve Model serialisation (which would fail because the 
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
		// Retrieve all known Models as RecordReferences, and loop over them:
		List<RecordReference> modelRecRefs = recordStore.retrieveRecordReferences(new RecordsQuery(Model.MODEL_SCHEMA));
		
		// TODO
		List<Record> newModelRecs = new ArrayList<Record>();
		
		// TODO
		List<Schema> schemata = new ArrayList<Schema>();
		
		// TODO
		for(RecordReference modelRecRef : modelRecRefs)
			try
			{
				Model model = client.getModel(Model.MODEL_ID_COLUMN.retrieveValue(modelRecRef));
				
				// Create new Model record in Models table:
				newModelRecs.add(model.getModelRecord(client));
				
				// Remember schema for table rename and more TODO:
				schemata.addAll(model.getSchemata());
			}
			catch(UnknownModelException ume)
			{
				upgradeOps.addWarning("Failed to get Model instance for modelID " + Model.MODEL_ID_COLUMN.retrieveValue(modelRecRef) + ", corresponding tables will be deleted.");
			}
		
		// TODO Drop ...
		upgradeOps.dropTable(recordStore, getOldTableName(Model.MODEL_SCHEMA), true);
		upgradeOps.dropTable(recordStore, getOldTableName(Model.SCHEMA_SCHEMA), true);
		
		// Create new Models table and insert new records:
		recordStore.store(newModelRecs); // TODO also achieves renames the object col

		// TODO
		List<String> keepTables = new ArrayList<String>(recordStore.getProtectedTableNames());
		
		// TODO
		for(Schema schema : schemata)
		{
			// Store new schemata record:
			recordStore.store(schema.getMetaRecord(client)); // TODO also achieves adding new ... cols
			
			// Rename its table:
			String newName = client.getTableName(schema);
			String oldName = getOldTableName(schema);
			if(!newName.equals(oldName))
				upgradeOps.renameTable(recordStore, oldName, newName);
			
			// TODO
			keepTables.add(newName);
		}
		
		// Delete unknown tables (TODO unupgradable):
		for(String tableName : upgradeOps.getAllTableNames(recordStore))
			if(!keepTables.contains(tableName))
				upgradeOps.dropTable(recordStore, tableName, false);
	}
	
	protected abstract String getOldTableName(Schema schema);
	
}
