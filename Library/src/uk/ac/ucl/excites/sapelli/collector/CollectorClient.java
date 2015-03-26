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
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreCreator;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.EncryptionSettings;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;

/**
 * @author mstevens
 *
 */
public abstract class CollectorClient extends TransmissionClient implements StoreHandle.StoreUser
{
	
	// STATICS-------------------------------------------------------
	static public final long COLLECTOR_MANAGEMENT_MODEL_ID = TRANSMISSION_MANAGEMENT_MODEL_ID + 1; // = 1
	
	//static public final Source ALL_COLLECTOR_RECORDS = Source.NotFrom(Transmission.)
	
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
		return super.getTableName(schema);
	}
	
	/**
	 * @param modelID
	 * @return the project corresponding to the given modelID, or null if no such project was found or if no projectStore is available
	 */
	public Project getProject(long modelID)
	{
		try
		{
			return projectStoreHandle.getStore(this).retrieveProject(GetProjectID(modelID), GetProjectFingerPrint(modelID));
		}
		catch(Exception e)
		{
			return null;
		}
		finally
		{
			projectStoreHandle.doneUsing(this);
		}
	}
	
	/**
	 * @param schema
	 * @return the form that is backed by the given schema
	 * @throws UnknownModelException when no matching Form is found
	 */
	public Form getForm(Schema schema) throws UnknownModelException
	{
		if(schema.isInternal())
			throw new IllegalArgumentException("Internal schema cannot be associated with a Form");
		Project project = getProject(schema.getModelID());
		if(project != null)
			for(Form f : project.getForms())
				if(f.getSchema().equals(schema))
					return f;
		throw new UnknownModelException(schema.getModelID());
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
			throw new UnknownModelException(modelID);
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

	@Override
	public void recordInserted(Record record)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void recordUpdated(Record record)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void recordDeleted(Record record)
	{
		// TODO Auto-generated method stub

	}
	
	@Override
	public void recordDeleted(RecordReference recordReference)
	{
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#recordsDeleted(uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery, int)
	 */
	@Override
	public void recordsDeleted(RecordsQuery query, int numberOfDeletedRecords)
	{
		// TODO Auto-generated method stub
		
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
	public Payload createPayload(int nonBuiltinType)
	{
		return null; // for now there are no Sapelli Collector-specific transmission payloads
	}

	@Override
	public Set<Column<?>> getNonTransmittableColumns(Schema schema)
	{
		return Collections.<Column<?>>emptySet(); // TODO pass transmission & export related columns
	}

}
