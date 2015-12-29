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

import java.util.HashMap;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.storage.db.sql.upgrades.Beta17UpgradeStep;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
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

	/**
	 * Applies all upgrades of {@link Beta17UpgradeStep} along with the change of the
	 * MediaField column type, and the renaming of existing MediaField attachment files.
	 * 
	 * @author mstevens
	 * 
	 * @see Beta17UpgradeStep
	 */
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
			if(schema == TransmissionStore.SENT_TRANSMISSION_SCHEMA)
				return "Sent_Transmissions";
			if(schema == TransmissionStore.SENT_TRANSMISSION_PART_SCHEMA)
				return "Sent_Transmission_Parts";
			if(schema == TransmissionStore.RECEIVED_TRANSMISSION_SCHEMA)
				return "Received_Transmissions";
			if(schema == TransmissionStore.RECEIVED_TRANSMISSION_PART_SCHEMA)
				return "Received_Transmission_Parts";
			if(schema == TransmissionStore.CORRESPONDENT_SCHEMA)
				return "Correspondents";
			if(schema == TransmissionStore.TRANSMITTABLE_RECORDS_SCHEMA)
				return "TransmitableRecords";
			// From old StorageClient:
			if(schema == Model.MODEL_SCHEMA)
				return "Models";
			if(schema == Model.SCHEMA_SCHEMA)
				return "Schemata";
			else
				return "Table_" + schema.getModelID() + '_' + schema.getModelSchemaNumber(); // we don't use schema#name to avoid name clashes and illegal characters
		}

		@Override
		protected void customiseTableConverter(Schema newSchema, final TableConverter tableConverter) throws DBException
		{
			// Find the form the schema is backing (if it is):
			Form form = null;
			try
			{
				form = client.getForm(newSchema);
			}
			catch(Exception e) {}
			
			// Check if we have a form:
			if(form == null)
				return;
			
			// Just to be sure:
			if(!newSchema.equals(form.getSchema()))
				throw new DBException("Form schema mismatch!");
			
			// Find MediaFields and store them in a mapping of column name to field...
			final Map<String, MediaField> mediaFields = new HashMap<String, MediaField>();
			for(Field field : form.getFields())
				if(field instanceof MediaField)
					mediaFields.put(field.getColumn().name, (MediaField) field);
			
			// If there are no media fields we don't need to replace any columns:
			if(mediaFields.isEmpty())
				return;
			
			// Return ColumnsReplacer which will generate oldSchema with the v1x MediaField columns
			//	and convert oldRecords to new ones (as well as rename the media attachment files):
			tableConverter.addColumnReplacer(new ColumnReplacer()
			{
				@Override
				public boolean matches(Column<?> newColumn)
				{
					return mediaFields.containsKey(newColumn.name);
				}
				
				@Override
				protected Column<?> getOldColumn(Column<?> newColumn)
				{
					return mediaFields.get(newColumn.name).createV1XColumn();
				}
				
				@Override
				protected Object convertValue(Column<?> newColumn, Record oldRecord)
				{
					// Convert value & rename attachment files:
					return mediaFields.get(newColumn.name).convertV1XColumnValue(oldRecord, true, fileStorageProvider);
				}
			});
		}
	
	}

}
