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

package uk.ac.ucl.excites.sapelli.collector.db;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;

/**
 * @author mstevens
 *
 */
public class CollectorSQLRecordStoreUpgrader extends SQLRecordStoreUpgrader<CollectorClient> implements StoreUser
{
	
	public CollectorSQLRecordStoreUpgrader(CollectorClient client, UpgradeCallback callback, FileStorageProvider fileStorageProvider)
	{
		super(	client,
				callback,
				fileStorageProvider.getOldDBVersionsFolder(false),
				// Steps:
				upgradeFrom2To3
				/*...*/);
	}
	
	/**
	 * TODO
	 * 
	 * 			//	The RecordReferences contain enough information (all we really need is the modelID) and 
			//	retrieving them does not involve Model serialisation (which would fail because the 
	 * 
	 * @author mstevens
	 */
	static private UpgradeStep<CollectorClient> upgradeFrom2To3 = new UpgradeStep<CollectorClient>(CollectorClient.COLLECTOR_RECORDSTORE_V2, CollectorClient.COLLECTOR_RECORDSTORE_V3)
	{
		@Override
		public void apply(CollectorClient client, SQLRecordStore<?, ?, ?> recordStore) throws Exception
		{
			// Retrieve all known Models as RecordReferences, and loop over them:
			for(RecordReference modelRecRef : recordStore.retrieveRecordReferences(new RecordsQuery(Model.MODEL_SCHEMA)))
			{
				Model model = null;
				try
				{
					model = client.getModel(Model.MODEL_ID_COLUMN.retrieveValue(modelRecRef));
					recordStore.store(model.getModelRecord(client));
				}
				catch(UnknownModelException ume)
				{
					// TODO
				}
				catch(Exception e)
				{
					//if(model == null)
						//client.logError("Failed to Failed to , throwable);
					// TODO recordStore.delete(modelRecRef); // bad idea??
					throw e;
				}
			}
			
			// TODO rename object col
			// TODO drop hashCode col? (doesn't hurt to keep it)
		}
	};
	
}
