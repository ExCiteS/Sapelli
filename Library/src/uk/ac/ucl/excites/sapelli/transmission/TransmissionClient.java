/**'
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

package uk.ac.ucl.excites.sapelli.transmission;

import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreCreator;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreSetter;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.StorageObserver;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;

/**
 * @author mstevens
 *
 */
public abstract class TransmissionClient extends StorageClient
{

	// STATICS-------------------------------------------------------
	/**
	 * ID for the reserved Transmission Management Model ({@link TransmissionStore#TRANSMISSION_MANAGEMENT_MODEL})
	 */
	static public final long TRANSMISSION_MANAGEMENT_MODEL_ID = 0;
	static
	{
		AddReservedModel(TransmissionStore.TRANSMISSION_MANAGEMENT_MODEL);
	}
	
	/**
	 * Flag indicating that a Schema has been defined at the Transmission layer of the Sapelli Library
	 */
	static private final int SCHEMA_FLAG_TRANSMISSION_LAYER =	1 << 6;
	
	/**
	 * Schema flag indicating that records of the Schema can be transmitted using the Transmission/Payload classes
	 */
	static public final int SCHEMA_FLAG_TRANSMITTABLE = 		1 << 7;
	
	// Note: flag bits 8 & 9 are reserved for future Transmission layer usage
	
	/**
	 * Flags used on "internal" Transmission layer Schemata
	 */
	static public final int SCHEMA_FLAGS_TRANSMISSION_INTERNAL = SCHEMA_FLAG_TRANSMISSION_LAYER;
	
	/**
	 * Create new Schema with the given name and adds it to the given model.
	 * 
	 * @param model
	 * @param name (will also be used as unprefixed tableName)
	 * @param unprefixedTableName
	 * @return
	 */
	static public Schema CreateTransmissionSchema(Model model, String name)
	{
		return CreateTransmissionSchema(model, name, name, model.getDefaultSchemaFlags());
	}
	
	/**
	 * Create new Schema with the given name and adds it to the given model.
	 * The given unprefixed table name is use to generate a complete table name (prefixed to indicate it is a Transmission layer table).
	 * 
	 * @param model
	 * @param name
	 * @param unprefixedTableName
	 * @return
	 */
	static public Schema CreateTransmissionSchema(Model model, String name, String unprefixedTableName)
	{
		return CreateTransmissionSchema(model, name, unprefixedTableName, model.getDefaultSchemaFlags());
	}
	
	/**
	 * Create new Schema with the given name and adds it to the given model.
	 * 
	 * @param model
	 * @param name (will also be used as unprefixed tableName)
	 * @param schemaFlags
	 * @return
	 */
	static public Schema CreateTransmissionSchema(Model model, String name, int schemaFlags)
	{
		return CreateTransmissionSchema(model, name, name, schemaFlags);
	}
	
	/**
	 * Create new Schema with the given name and adds it to the given model.
	 * The given unprefixed table name is use to generate a complete table name (prefixed to indicate it is a Transmission layer table).
	 * 
	 * @param model
	 * @param name
	 * @param unprefixedTableName
	 * @param schemaFlags
	 * @return
	 */
	static public Schema CreateTransmissionSchema(Model model, String name, String unprefixedTableName, int schemaFlags)
	{
		return new Schema(model, name, GetTransmissionPrefixedSchemaTableName(unprefixedTableName, schemaFlags), schemaFlags);
	}
	
	/**
	 * Generates a complete table name from the given unprefixed table name (prefixed to indicate it is a Transmission layer table).
	 * 
	 * @param unprefixedTableName
	 * @param schemaFlags
	 * @return the full table name
	 */
	static public String GetTransmissionPrefixedSchemaTableName(String unprefixedTableName, int schemaFlags)
	{
		if(!TestSchemaFlags(schemaFlags, SCHEMA_FLAG_TRANSMISSION_LAYER))
			throw new IllegalArgumentException("SCHEMA_FLAG_TRANSMISSION flag expected to be set");
		// Build tableName:
		StringBuilder tableNameBldr = new StringBuilder("Transmission_");
		tableNameBldr.append(unprefixedTableName);
		// Return full table name:
		return tableNameBldr.toString();
	}

	// DYNAMICS------------------------------------------------------
	public final StoreHandle<TransmissionStore> transmissionStoreHandle = new StoreHandle<TransmissionStore>(new StoreCreator<TransmissionStore>()
	{
		@Override
		public void createAndSetStore(StoreSetter<TransmissionStore> setter) throws DBException
		{
			setter.setAndInitialise(new TransmissionStore(TransmissionClient.this));
		}
	});
	
	public TransmissionClient()
	{
		new TransmissionStorageObserver(); // no need to hold a reference to it, the object will register itself as a StorageObserver
	}
	
	/**
	 * TODO
	 * 
	 * @param nonBuiltinType
	 * @return
	 */
	public abstract Payload createCustomPayload(int nonBuiltinType);
	
	/**
	 * Override to add support for receiving custom payload or the change handling of built-in payload types
	 * 
	 * @return
	 */
	public TransmissionController.PayloadReceiver getCustomPayloadReceiver()
	{
		return null;
	}
	
	/**
	 * TODO
	 * 
	 * @param schema
	 * @return
	 */
	public abstract List<Correspondent> getReceiversFor(Schema schema);
	
	/**
	 * Returns columns from ther given schema that should not be transmitted.
	 * It is assumed these are optional columns, or (TODO once this is supported) non-optional columns with a default value.
	 * 
	 * @param schema
	 * @return
	 */
	public abstract Set<Column<?>> getNonTransmittableColumns(Schema schema);
	
	/**
	 * Helper class to receive store event updates and let TransmissionStore update its "TransmitableRecords" table accordingly. 
	 * 
	 * @author mstevens
	 */
	private final class TransmissionStorageObserver implements StorageObserver, StoreUser
	{
		
		private TransmissionStore tStore;
		
		public TransmissionStorageObserver()
		{
			/* Note:
			 * Do *not* initialise tStore here as it causes a call to CollectorApp#getFileStorageProvider()
			 * before CollectorApp#initialiseFileStorage() has been called. */
			
			// Register ourself as an observer to receive updates about storage events:
			addObserver(this);
		}
		
		private boolean init()
		{
			if(tStore == null)
			{
				try
				{
					tStore = transmissionStoreHandle.getStore(this);
				}
				catch(DBException e)
				{
					e.printStackTrace(System.err); // TODO propagate upwards (to android) error logging
					return false;
				}
			}
			return true;
		}
		
		@Override
		public void storageEvent(RecordOperation operation, RecordReference recordRef)
		{
			if(init() /*make sure we have tStore*/ && recordRef.getReferencedSchema().hasFlags(SCHEMA_FLAG_TRANSMITTABLE))
				receiverLoop : for(Correspondent receiver : getReceiversFor(recordRef.getReferencedSchema()))
				{
					switch(operation)
					{
						case Inserted :
						case Updated :
							tStore.storeTransmittableRecord(receiver, recordRef, null); //TODO will this wipe tosend rec's for same rec that already had a transmission?
							break;
						case Deleted :
							tStore.deleteTransmittableRecord(recordRef); // record will be forgotten about for each receiver ...
							break receiverLoop; // ... so we are done here
						default :
							throw new IllegalArgumentException("Unknown " + RecordOperation.class.getSimpleName());
					}
				}
		}
		
		@Override
		public void finalize()
		{
			transmissionStoreHandle.doneUsing(this);
		}
		
	}

}
