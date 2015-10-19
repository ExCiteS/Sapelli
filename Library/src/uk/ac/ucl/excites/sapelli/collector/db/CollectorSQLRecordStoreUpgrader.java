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
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.storage.db.sql.upgrades.Beta17UpgradeStep;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * @author mstevens
 *
 */
public class CollectorSQLRecordStoreUpgrader extends SQLRecordStoreUpgrader implements StoreUser
{
	
	public CollectorSQLRecordStoreUpgrader(CollectorClient client, UpgradeCallback callback, FileStorageProvider fileStorageProvider)
	{
		super(	callback,
				fileStorageProvider.getOldDBVersionsFolder(false),
				// Steps:
				//	v2->v3:
				CollectorBeta17UpgradeStep(client)
				/*...*/);
	}

	static private UpgradeStep<CollectorClient> CollectorBeta17UpgradeStep(CollectorClient client)
	{
		return new Beta17UpgradeStep<CollectorClient>(client, CollectorClient.COLLECTOR_RECORDSTORE_V2, CollectorClient.COLLECTOR_RECORDSTORE_V3)
		{
			@Override
			protected String getOldTableName(Schema schema)
			{
				// From old CollectorClient:
				if(schema == ProjectRecordStore.PROJECT_SCHEMA)
					return "Collector_Projects";
				if(schema == ProjectRecordStore.FSI_SCHEMA)
					return "Project_FormSchemaInfo";
				if(schema == ProjectRecordStore.HFK_SCHEMA)
					return "Relationship_HFKs";
				// From old TransmissionClient:
				if(schema == TransmissionStore.TRANSMISSION_SCHEMA)
					return "Transmissions";
				if(schema == TransmissionStore.TRANSMISSION_PART_SCHEMA)
					return "Transmission_Parts";
				// From old StorageClient:
				if(schema == Model.MODEL_SCHEMA)
					return "Models";
				if(schema == Model.SCHEMA_SCHEMA)
					return "Schemata";
				else
					return "Table_" + schema.getModelID() + '_' + schema.getModelSchemaNumber(); // we don't use schema#name to avoid name clashes and illegal characters
			}
		};
	}
	
}
