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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.db.CollectorSQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectRecordStore;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreCreator;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreOperation;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreOperationWithReturn;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreOperationWithReturnNoException;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreSetter;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.StreamHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.sql.upgrades.Beta17UpgradeStep;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
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
	/**
	 * Version used in all Sapelli Collector v2.0 pre-releases up to and including Beta 16:
	 */
	static public final int COLLECTOR_RECORDSTORE_V2 = 2;
	
	/**
	 * Version used from Sapelli Collector v2.0 Beta 17.
	 * 
	 * @see Beta17UpgradeStep
	 * @see CollectorSQLRecordStoreUpgrader
	 */
	static public final int COLLECTOR_RECORDSTORE_V3 = 3;
	
	static public final int CURRENT_COLLECTOR_RECORDSTORE_VERSION = COLLECTOR_RECORDSTORE_V3;
	
	/**
	 * Flag indicating that a Schema has been defined at the Collector layer of the Sapelli Library.
	 * 
	 * Note: flag bits 11, 12 & 13 are reserved for future Collector layer usage
	 */
	static private final int SCHEMA_FLAG_COLLECTOR_LAYER =		1 << 10;
	
	/**
	 * Flags used on "internal" Collector layer Schemata.
	 */
	static public final int SCHEMA_FLAGS_COLLECTOR_INTERNAL = 	SCHEMA_FLAG_COLLECTOR_LAYER;
	
	/**
	 * Flags used on Schemata for all Collector data records.
	 */
	static public final int SCHEMA_FLAGS_COLLECTOR_DATA = 		SCHEMA_FLAG_COLLECTOR_LAYER | SCHEMA_FLAG_EXPORTABLE | SCHEMA_FLAG_TRANSMITTABLE;
	
	/**
	 * Flags used on Schemata for automatically-generated ("auxiliary") Collector data records.
	 */
	static public final int SCHEMA_FLAGS_COLLECTOR_AUX_DATA = 	SCHEMA_FLAGS_COLLECTOR_DATA;
	
	/**
	 * Flags used on Schemata for user-generated Collector data records.
	 */
	static public final int SCHEMA_FLAGS_COLLECTOR_USER_DATA = 	SCHEMA_FLAGS_COLLECTOR_DATA | SCHEMA_FLAG_KEEP_HISTORY;
	
	/**
	 * ID for the reserved Collector Management Model ({@link ProjectRecordStore#COLLECTOR_MANAGEMENT_MODEL})
	 */
	static public final long COLLECTOR_MANAGEMENT_MODEL_ID = TRANSMISSION_MANAGEMENT_MODEL_ID + 1; // = 1
	
	// Add tableName prefixes & reserved model (in that order!):
	static
	{
		AddTableNamePrefix(SCHEMA_FLAG_COLLECTOR_LAYER, "Collector_");
		AddTableNamePrefix(SCHEMA_FLAGS_COLLECTOR_DATA, "Data_");
		AddReservedModel(ProjectRecordStore.COLLECTOR_MANAGEMENT_MODEL);
	}
		
	/**
	 * Returns the modelID to use for the {@link Model} of the given {@link Project} or {@link ProjectDescriptor}.  
	 * 
	 * @param projDescr
	 * @return unsigned 56 bit integer
	 * @throws IllegalArgumentException in case of a clash with a reserved Model
	 */
	static public long GetModelID(ProjectDescriptor projDescr) throws IllegalArgumentException
	{
		long modelID =	((((long) projDescr.getFingerPrint()) & 0xffffffffl) << Project.PROJECT_ID_SIZE) +	// Project finger print takes up first 32 bits
						projDescr.getID();	
		if(GetReservedModel(modelID) != null)
			throw new IllegalArgumentException("Model ID computed for Project \"" + projDescr.toString(false) + "\" clashes with reserved model ID (" + modelID + ")!");
		return modelID;
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
	
	static protected final byte MODEL_SERIALISATION_KIND_COMPRESSED_COLLECTOR_PROJECT_XML = MODEL_SERIALISATION_KIND_RESERVED + 1;
	
	// DYNAMICS------------------------------------------------------
	public final StoreHandle<ProjectStore> projectStoreHandle = new StoreHandle<ProjectStore>(new StoreCreator<ProjectStore>()
	{
		@Override
		public void createAndSetStore(StoreSetter<ProjectStore> setter) throws DBException
		{
			createAndSetProjectStore(setter);
		}
	});
	
	/**
	 * Creates a new ProjectStore instance
	 * 
	 * @param setter
	 * @throws DBException
	 */
	protected abstract void createAndSetProjectStore(StoreSetter<ProjectStore> setter) throws DBException;
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#serialiseClientModel(uk.ac.ucl.excites.sapelli.storage.model.Model, java.io.OutputStream)
	 */
	@Override
	protected void serialiseClientModel(Model model, final OutputStream out) throws IOException, UnknownModelException
	{
		final Project project = getProject(model.id);
		if(project != null)
		{
			// Write "kind" byte:
			out.write(MODEL_SERIALISATION_KIND_COMPRESSED_COLLECTOR_PROJECT_XML);
			// Serialise project and write compressed result to the OutputStream:
			projectStoreHandle.executeNoDBEx(new StoreOperation<ProjectStore, IOException>()
			{
				@Override
				public void execute(ProjectStore store) throws IOException
				{
					OutputStream cOut = null;
					try
					{
						cOut = compress(out);
						store.serialise(project, cOut);
					}
					finally
					{
						StreamHelpers.SilentFlushAndClose(cOut);
					}
				}
			});
		}
		else
			throw new UnknownModelException(model.id, model.getName());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#deserialiseClientModel(byte, java.io.InputStream)
	 */
	@Override
	protected Model deserialiseClientModel(byte kind, final InputStream in) throws Exception
	{
		if(kind == MODEL_SERIALISATION_KIND_COMPRESSED_COLLECTOR_PROJECT_XML)
		{
			Project project = projectStoreHandle.executeWithReturn(new StoreOperationWithReturn<ProjectStore, Project, Exception>()
			{
				@Override
				public Project execute(ProjectStore store) throws IOException
				{
					InputStream dcIn = null;
					try
					{
						dcIn = decompress(in);
						return store.deserialise(dcIn);
					}
					finally
					{
						StreamHelpers.SilentClose(dcIn);
					}
				}
			});
			if(project != null)
				return project.getModel();
		}
		// else:
		return null;
	}
	
	/**
	 * @param model
	 * @return the project corresponding to the given model, or {@code null} if the model was {@code null), if no such project was found, or if no projectStore is available
	 */
	public Project getProject(Model model)
	{
		if(model == null)
			return null;
		else
			return getProject(model.id);
	}
	
	/**
	 * @param modelID
	 * @return the project corresponding to the given modelID, or {@code null} if no such project was found or if no projectStore is available
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
		return getForm(getProject(schema.getModelID()), schema);
	}
	
	/**
	 * @param project
	 * @param schema
	 * @return the form that is backed by the given schema
	 * @throws UnknownModelException when no matching Form is found
	 */
	public Form getForm(Project project, Schema schema) throws UnknownModelException
	{
		if(project != null)
			for(Form f : project.getForms())
				if(f.isProducesRecords() && f.getSchema().equals(schema))
					return f;
		throw new UnknownModelException(schema.getModelID(), schema.getModel().getName());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getClientModel(long)
	 */
	@Override
	protected Model getClientModel(long modelID)
	{
		Project project = getProject(modelID);
		if(project != null)
			return project.getModel();
		else
			return null;
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
	public Payload createPayload(int nonBuiltinType)
	{
		return null; // for now there are no Sapelli Collector-specific transmission payloads
	}

	@Override
	public Set<Column<?>> getNonTransmittableColumns(Schema schema)
	{
		// TODO get rid of this
		return Collections.<Column<?>>emptySet(); // TODO pass transmission & export related columns
	}

}
