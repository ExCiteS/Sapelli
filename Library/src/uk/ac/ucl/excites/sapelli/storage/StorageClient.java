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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreCreator;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapOutputStream;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
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
	
	static private final Charset UTF8 = Charset.forName("UTF-8");
	
	static protected final byte[] NEW_MODEL_SERIALISATION_HEADER = "SapelliModel".getBytes(UTF8);
	
	static private final StringColumn NEW_MODEL_SERIALISATION_KIND_COLUMN = StringColumn.ForCharacterCount("Kind", false, 15, UTF8);

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
		Model reservedModel = getReservedModel(modelID);
		if(reservedModel != null)
			return reservedModel;
		// Get client model:
		return getClientModel(modelID);
	}
	
	private Model getReservedModel(long modelID)
	{
		for(Model model : getReservedModels())
			if(model.getID() == modelID)
				return model;
		return null;
	}
	
	/**
	 * Subclasses can override this but *must* return at least the same models returned by the super implementation.
	 * 
	 * @return
	 */
	protected List<Model> getReservedModels()
	{
		return new ArrayList<Model>();
	}
	
	/**
	 * @param modelID
	 * @return
	 * @throws UnknownModelException
	 */
	protected abstract Model getClientModel(long modelID) throws UnknownModelException;
	
	/**
	 * TODO 
	 * @param model
	 * @param out
	 * @throws IOException
	 */
	public void serialiseModel(Model model, OutputStream out) throws IOException
	{
		// Write header:
		//out.write(NEW_MODEL_SERIALISATION_HEADER);
		
		// Check if this is a reserved model:
		if(false) //getReservedModel(model.id) != null)
		{	// Write "Reserved" + the model id:
			//out.write(NEW_MODEL_SERIALISATION_KIND_COLUMN.toBytes("Reserved"));
			
			
			BitWrapOutputStream bitOut = new BitWrapOutputStream(out);
			Model.MODEL_ID_FIELD.write(model.id, bitOut);
			bitOut.flush();
			bitOut.close();
		}
		else
		{	// Use Java object serialisation:
			ObjectOutputStream objOut = new ObjectOutputStream(out);
			objOut.writeObject(model);
			objOut.flush();
			objOut.close();
		}
	}
	
	/**
	 * TODO
	 * 
	 * @param in
	 * @return
	 * @throws UnknownModelException
	 * @throws IOException
	 * @throws ClassNotFoundException
	 */
	public Model deserialiseModel(InputStream in) throws UnknownModelException, IOException, ClassNotFoundException
	{
		// Check if this is a reserved model:
		/*if(!in.markSupported())
			in = new BufferedInputStream(in); // decorate stream so we can use mark/reset		
		
		// Check new header:
		in.mark(NEW_MODEL_SERIALISATION_HEADER.length);
		byte[] newHeaderBytes = new byte[NEW_MODEL_SERIALISATION_HEADER.length];
		int read = in.read(newHeaderBytes);
		if(read == NEW_MODEL_SERIALISATION_HEADER.length && Arrays.equals(newHeaderBytes, NEW_MODEL_SERIALISATION_HEADER))
		{	// New header present...
			// Read kind:
			//NEW_MODEL_SERIALISATION_KIND_COLUMN.fromBytes(bytes)
			return null;
		}
		else
		{
			in.reset();*/
			// Use Java object serialisation:
			return deserialiseModelObject(in);
		//}
	}
	
	private Model deserialiseModelObject(InputStream in) throws ClassNotFoundException, IOException
	{
		ObjectInputStream objIn = new ObjectInputStream(in);
		try
		{
			return (Model) objIn.readObject();
		}
		finally
		{
			objIn.close();
		}
	}
	
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
