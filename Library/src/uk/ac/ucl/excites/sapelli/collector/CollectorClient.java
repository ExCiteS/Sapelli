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

package uk.ac.ucl.excites.sapelli.collector;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectRecordStore;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendingSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreCreator;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreOperationWithReturnNoException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.EncryptionSettings;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;

/**
 * @author mstevens
 *
 */
public abstract class CollectorClient extends TransmissionClient implements StoreHandle.StoreUser
{
	
	// STATICS-------------------------------------------------------
	/**
	 * Version used in all Sapelli Collector v2.0 pre-releases up to and including Beta 14:
	 */
	static public final int COLLECTOR_RECORDSTORE_V2 = 2;
	
	static public final int CURRENT_COLLECTOR_RECORDSTORE_VERSION = COLLECTOR_RECORDSTORE_V2;
	
	static public final long COLLECTOR_MANAGEMENT_MODEL_ID = TRANSMISSION_MANAGEMENT_MODEL_ID + 1; // = 1
	
	/**
	 * Flag indicating that a Schema has been defined at the Collector layer of the Sapelli Library
	 */
	static private final int SCHEMA_FLAG_COLLECTOR_LAYER =		1 << 10;
	
	// Note: flag bits 11, 12 & 13 are reserved for future Collector layer usage
	
	/**
	 * Flags used on "internal" Collector layer Schemata
	 */
	static public final int SCHEMA_FLAGS_COLLECTOR_INTERNAL = 	SCHEMA_FLAG_COLLECTOR_LAYER;
	
	/**
	 * Flags used on Schemata for all Collector data records
	 */
	static public final int SCHEMA_FLAGS_COLLECTOR_DATA = 		SCHEMA_FLAG_COLLECTOR_LAYER | SCHEMA_FLAG_EXPORTABLE;
	
	/**
	 * Flags used on Schemata for automatically-generated ("auxiliary") Collector data records
	 */
	static public final int SCHEMA_FLAGS_COLLECTOR_AUX_DATA = 	SCHEMA_FLAGS_COLLECTOR_DATA;
	
	/**
	 * Flags used on Schemata for user-generated Collector data records
	 */
	static public final int SCHEMA_FLAGS_COLLECTOR_USER_DATA = 	SCHEMA_FLAGS_COLLECTOR_DATA | SCHEMA_FLAG_KEEP_HISTORY;
	
	/**
	 * @param project
	 * @return unsigned 56 bit integer
	 */
	static public long GetModelID(Project project)
	{
		return	((((long) project.getFingerPrint()) & 0xffffffffl) << Project.PROJECT_ID_SIZE) + // Project finger print takes up first 32 bits
				project.getID();																 // Project id takes up next 24 bits
	}
	
	/**
	 * @param modelID
	 * @return project ID (24 bit unsigned int)
	 */
	static public int GetProjectID(long modelID)
	{
		return (int) (modelID % (1 << Project.PROJECT_ID_SIZE));
	}
	
	/**
	 * @param modelID
	 * @return project fingerprint (32 bit signed int)
	 */
	static public int GetProjectFingerPrint(long modelID)
	{
		return (int) (modelID >> Project.PROJECT_ID_SIZE);
	}
	
	// DYNAMICS------------------------------------------------------
	public final StoreHandle<ProjectStore> projectStoreHandle = new StoreHandle<ProjectStore>(new StoreCreator<ProjectStore>()
	{
		@Override
		public ProjectStore createStore() throws DBException
		{
			return createProjectStore();
		}
	});
	
	/**
	 * Returns a new ProjectStore instance
	 * 
	 * @return
	 * @throws DBException
	 */
	protected abstract ProjectStore createProjectStore() throws DBException;
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getReserveredModels()
	 */
	@Override
	public List<Model> getReservedModels()
	{
		List<Model> reserved = super.getReservedModels();
		reserved.add(ProjectRecordStore.COLLECTOR_MANAGEMENT_MODEL);
		return reserved;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getTableName(uk.ac.ucl.excites.sapelli.storage.model.Schema)
	 */
	@Override
	public String getTableName(Schema schema)
	{
		if(schema == ProjectRecordStore.PROJECT_SCHEMA)
			return "Collector_Projects";
		if(schema == ProjectRecordStore.FSI_SCHEMA)
			return "Project_FormSchemaInfo";
		if(schema == ProjectRecordStore.HFK_SCHEMA)
			return "Relationship_HFKs";
		if(schema == ProjectRecordStore.SEND_RECORDS_SCHEDULE_SCHEMA)
			return "Project_TransmissionSchedule";
		return super.getTableName(schema);
	}
	
	/**
	 * @param modelID
	 * @return the project corresponding to the given modelID, or null if no such project was found or if no projectStore is available
	 */
	public Project getProject(final long modelID)
	{
		return projectStoreHandle.executeWithReturnNoDBEx(new StoreOperationWithReturnNoException<ProjectStore, Project>()
		{
			@Override
			public Project execute(ProjectStore store)
			{
				return store.retrieveProject(GetProjectID(modelID), GetProjectFingerPrint(modelID));
			}
		});
	}
	
	/**
	 * @param schema
	 * @return the form that is backed by the given schema
	 * @throws UnknownModelException when no matching Form is found
	 */
	public Form getForm(Schema schema) throws UnknownModelException
	{
		Project project = getProject(schema.getModelID());
		if(project != null)
			for(Form f : project.getForms())
				if(f.getSchema().equals(schema))
					return f;
		throw new UnknownModelException(schema.getModelID(), schema.getModel().getName());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getClientModel(long)
	 */
	@Override
	protected Model getClientModel(long modelID) throws UnknownModelException
	{
		// TODO check record store too? (requires retrieveModel method)
		Project project = getProject(modelID);
		if(project != null)
			return project.getModel();
		else
			throw new UnknownModelException(modelID, null);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getSchemaV1(int, int)
	 */
	@Override
	public Schema getSchemaV1(int schemaID, int schemaVersion) throws UnknownModelException
	{
		try
		{
			Project project = projectStoreHandle.getStore(this).retrieveV1Project(schemaID, schemaVersion); // can throw NPE or DBException
			// return schema of the first (and assumed only) form:
			return project.getForm(0).getSchema(); // can throw NPE
		}
		catch(Exception e)
		{	// regardless of whether it is an NPE, DBException or another Exception:
			throw new UnknownModelException(schemaID, schemaVersion);
		}
		finally
		{
			projectStoreHandle.doneUsing(this);
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.TransmissionClient#getEncryptionSettingsFor(Model)
	 */
	//@Override
	public EncryptionSettings getEncryptionSettingsFor(Model model)
	{
		/*TODO FIX THIS
		 * This is buggy/hacky! Because schema's can be shared by multiple forms (and no schema ID/version duplicates are allowed)
		 * we cannot safely determine transmission settings based on the schema id/version.
		 */
//		List<Form> forms = dao.retrieveForms(schema.getID(), schema.getVersion());
//		if(!forms.isEmpty())
//		{
//			if(forms.get(0)/*HACK!*/.getProject() != null)
//				return forms.get(0).getProject().getTransmissionSettings();
//			else
//				return null;
//		}
//		else
//		{
//			return null;
//		}
		return null;
	}

	@Override
	public Payload createCustomPayload(int nonBuiltinType)
	{
		return null; // for now there are no Sapelli Collector-specific transmission payloads
	}

	@Override
	public List<Correspondent> getReceiversFor(final Schema schema)
	{
		try
		{
			// Get project:
			final Project project = getForm(schema).getProject(); // throws UnknownModelException
			
			// Get stores:
			TransmissionStore tStore = transmissionStoreHandle.getStore(this);
			ProjectStore pStore = projectStoreHandle.getStore(this);
		
			// Get schedule (currently we support only 1 per project):
			SendingSchedule schedule = pStore.retrieveSendScheduleForProject(project, tStore);
			
			// Get receiving correspondent and return as list:
			return schedule != null && schedule.getReceiver() != null /*just in case*/ ?
				Collections.singletonList(schedule.getReceiver()) :
				Collections.<Correspondent> emptyList();
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err); // TODO log error
			return Collections.<Correspondent> emptyList();
		}	
		finally
		{
			transmissionStoreHandle.doneUsing(this);
			projectStoreHandle.doneUsing(this);
		}
	}

	@Override
	public Set<Column<?>> getNonTransmittableColumns(Schema schema)
	{
		// TODO get rid of this
		return Collections.<Column<?>>emptySet(); // TODO pass transmission & export related columns
	}

}
