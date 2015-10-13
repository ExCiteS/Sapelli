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

package uk.ac.ucl.excites.sapelli.storage;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreCreator;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;

/**
 * TODO upwards error logging mechanism 
 * 
 * @author mstevens
 */
public abstract class StorageClient implements StorageObserver
{

	// STATICS ------------------------------------------------------
	static public enum RecordOperation
	{
		Inserted,
		Updated,
		Deleted
	}
	
	/**
	 * Schema flag indicating that the Schema has been defined at the Storage layer of the Sapelli Library 
	 */
	static private final int SCHEMA_FLAG_STORAGE_LAYER =	1 << 0;
	
	/**
	 * Schema flag indicating that records of the Schema are exportable
	 */
	static public final int SCHEMA_FLAG_EXPORTABLE = 		1 << 1;
	
	/**
	 * Schema flag indicating that changes made to records of the Schema will reported to StorageObservers
	 */
	static public final int SCHEMA_FLAG_TRACK_CHANGES =		1 << 2;

	/**
	 * Schema flag indicating that records of the Schema hold a history of changes.
	 * Using this flag implies the Schema will have change tracking (@see {@link #SCHEMA_FLAG_TRACK_CHANGES}) enabled as well.
	 * TODO Not yet implemented
	 */
	static public final int SCHEMA_FLAG_KEEP_HISTORY =		SCHEMA_FLAG_TRACK_CHANGES | 1 << 3;
	
	// Note: flag bits 4 & 5 are reserved for future Storage layer usage
	
	/**
	 * Flags used on "internal" Storage layer Schemata 
	 */
	static public final int SCHEMA_FLAGS_STORAGE_INTERNAL =	SCHEMA_FLAG_STORAGE_LAYER;
	
	static private final Map<Long, Model> RESERVED_MODELS = new HashMap<Long, Model>();
	
	static public final Collection<Model> GetReservedModels()
	{
		return Collections.unmodifiableCollection(RESERVED_MODELS.values());
	}
	
	static final protected void AddReservedModel(Model newReservedModel)
	{
		if(newReservedModel == null)
			throw new NullPointerException("newReservedModel cannot be null!");
		if(RESERVED_MODELS.containsKey(newReservedModel.id) && RESERVED_MODELS.get(newReservedModel.id) != newReservedModel)
			throw new IllegalStateException("Reserved model id clash (id: " + newReservedModel.id + ")!");
		RESERVED_MODELS.put(newReservedModel.id, newReservedModel);
	}
	
	/**
	 * @param modelID
	 * @return a reserved Model with the given modelID, or {@code null} if no such reserved Model is known
	 */
	static final protected Model GetReservedModel(long modelID)
	{
		return RESERVED_MODELS.get(modelID);
	}

	// DYNAMICS -----------------------------------------------------
	private final List<StorageObserver> observers = new LinkedList<StorageObserver>();
	
	public final StoreHandle<RecordStore> recordStoreHandle = new StoreHandle<RecordStore>(new StoreCreator<RecordStore>()
	{
		@Override
		public RecordStore createStore() throws DBException
		{
			return createRecordStore();
		}
	});
	
	/**
	 * @param modelID
	 * @return
	 * @throws UnknownModelException
	 */
	public final Model getModel(long modelID) throws UnknownModelException
	{
		// First check reserved models:
		Model reservedModel = GetReservedModel(modelID);
		if(reservedModel != null)
			return reservedModel;
		// Get client model:
		return getClientModel(modelID);
	}
	
	/**
	 * @param modelID
	 * @return
	 * @throws UnknownModelException
	 */
	protected abstract Model getClientModel(long modelID) throws UnknownModelException;
	
	/**
	 * @param modelID
	 * @param schemaNumber
	 * @return
	 * @throws UnknownModelException when no model with the given {@code modelID} was found
	 * @throws IndexOutOfBoundsException when the model with the given {@code modelID} does not have a schema with the given {@code schemaNumber}
	 */
	public Schema getSchema(long modelID, int schemaNumber) throws UnknownModelException, IndexOutOfBoundsException
	{
		return getSchema(modelID, schemaNumber, null);
	}
	
	/**
	 * @param modelID
	 * @param schemaNumber
	 * @param schemaName may be null; is not checked against returned Schema(!), only used to pass to UnknownModelException in case no model is found 
	 * @return a matching Schema
	 * @throws UnknownModelException when no model with the given {@code modelID} was found
	 * @throws IndexOutOfBoundsException when the model with the given {@code modelID} does not have a schema with the given {@code schemaNumber}
	 */
	public Schema getSchema(long modelID, int schemaNumber, String schemaName) throws UnknownModelException, IndexOutOfBoundsException
	{
		try
		{
			return getModel(modelID).getSchema(schemaNumber);
		}
		catch(UnknownModelException ume)
		{
			throw new UnknownModelException(modelID, null, schemaNumber, schemaName); // throw UME with schema information instead of only modelID
		}
	}
	
	/**
	 * Returns the name to be used for a table which will contain records of the given schema in
	 * back-end (relational) database storage (i.e. through a RecordStore implementation).
	 * 
	 * May be overridden by subclasses to add additional exceptional cases.
	 * 
	 * TODO replace this and the overriding methods by a more elegant mechanism using the Schema.name (and perhaps making it plural)
	 * 
	 * @return
	 */
	public String getTableName(Schema schema)
	{
		if(schema == Model.MODEL_SCHEMA)
			return "Models";
		if(schema == Model.META_SCHEMA)
			return "Schemata";
		else
			return "Table_" + schema.getModelID() + '_' + schema.getModelSchemaNumber(); // we don't use schema#name to avoid name clashes and illegal characters
	}
	
	/**
	 * @param schemaID
	 * @param schemaVersion
	 * @return a matching {@link Schema} instance
	 * @throws {@link UnknownModelException} when no matching schema is found
	 */
	public abstract Schema getSchemaV1(int schemaID, int schemaVersion) throws UnknownModelException;
	
	public final void addObserver(StorageObserver observer)
	{
		if(observer != null)
			this.observers.add(observer);
	}
	
	@Override
	public final void storageEvent(RecordOperation operation, RecordReference recordRef)
	{
		// Forward to all observers (if any):
		for(StorageObserver observer : observers)
			// Only report events about records whose Schema has track changes enabled:
			if(recordRef.getReferencedSchema().hasFlags(SCHEMA_FLAG_TRACK_CHANGES))
				observer.storageEvent(operation, recordRef);
	}
	
	/**
	 * TODO this can be problematic, Transmission client needs to know which records...
	 * TODO remove
	 */
	public void recordsDeleted(RecordsQuery query, int numberOfDeletedRecords) {};
	
	/**
	 * Returns a new RecordStore instance
	 * 
	 * @return
	 * @throws DBException
	 */
	protected abstract RecordStore createRecordStore() throws DBException;
	
}
