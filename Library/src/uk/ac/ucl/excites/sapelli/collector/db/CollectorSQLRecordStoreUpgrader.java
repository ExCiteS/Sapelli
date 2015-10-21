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

import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
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
				new CollectorBeta17UpgradeStep(client, fileStorageProvider)
				/*...*/);
	}

	static private class CollectorBeta17UpgradeStep extends Beta17UpgradeStep<CollectorClient>
	{

		private final FileStorageProvider fileStorageProvider;
		
		public CollectorBeta17UpgradeStep(CollectorClient client, FileStorageProvider fileStorageProvider)
		{
			super(client, CollectorClient.COLLECTOR_RECORDSTORE_V2, CollectorClient.COLLECTOR_RECORDSTORE_V3);
			this.fileStorageProvider = fileStorageProvider;
		}
		
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

		@Override
		protected void clientSpecificUpgradeWork(SQLRecordStore<?, ?, ?> recordStore, UpgradeOperations upgradeOps, List<Schema> schemataWithTables) throws DBException
		{
			// TODO
		}
			
	}
	
//	@Override
//	public void upgrade(SQLRecordStore<?, ?, ?> recordStore, int oldVersion, int newVersion) throws DBException
//	{		
//		List<Record> convertedRecords = new ArrayList<Record>();
//		
//		// UPDATE V2 --> V3:
//		if(oldVersion < CollectorClient.COLLECTOR_RECORDSTORE_V3)
//		{
//			// Media-field column:
//			for(Project project : client.projectStoreHandle.getStore(this).retrieveProjects())
//			{
//				for(Form form : project.getForms())
//				{
//					// Find media fields:
//					List<MediaField> mediaFields = new ArrayList<MediaField>();
//					for(Field field : form.getFields())
//						if(field instanceof MediaField)
//							mediaFields.add((MediaField) field);
//					// Replace the columns of any media fields:
//					if(!mediaFields.isEmpty())
//					{
//						List<ColumnReplacer<?, ?>> replacers = new ArrayList<ColumnReplacer<?, ?>>(mediaFields.size());
//						for(MediaField mField : mediaFields)
//							replacers.add(new MediaColumnReplacer(mField.createV1XColumn(), mField.getColumn()));
//						replace(recordStore, form.getSchema(), replacers);
//					}
//				}
//			}
//		}
//		
//		// Persist dropped tables:
//		cleanup(recordStore);
//		
//		// Add converted records:
//		recordStore.store(convertedRecords);
//	}
	
//	private class MediaColumnReplacer extends ColumnReplacer<Long, List<Long>>
//	{
//
//		public MediaColumnReplacer(IntegerColumn oldColumn, IntegerListColumn newColumn)
//		{
//			super(oldColumn, newColumn);
//		}
//
//		@Override
//		public List<Long> convert(Long originalValue)
//		{
//			if(originalValue == null)
//				return null;
//			// originalValue = number of attachments
//			List<Long> offsets = new ArrayList<Long>(originalValue.intValue());
//			for(int i = 0; i < originalValue.intValue(); i++)
//				offsets.add(Long.valueOf(MediaField.V1X_ATTACHMENT_OFFSET));
//			return offsets;
//		}
//		
//	}
	
}
