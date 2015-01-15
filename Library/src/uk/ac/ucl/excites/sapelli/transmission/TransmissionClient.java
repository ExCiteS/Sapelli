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

package uk.ac.ucl.excites.sapelli.transmission;

import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreCreator;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.db.ReceivedTransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.db.SentTransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * @author mstevens
 *
 */
public abstract class TransmissionClient extends StorageClient
{

	// STATICS-------------------------------------------------------
	static public final long TRANSMISSION_MANAGEMENT_MODEL_ID = 0; // reserved!

	// DYNAMICS------------------------------------------------------
	public final StoreHandle<SentTransmissionStore> sentTransmissionStoreHandle = new StoreHandle<SentTransmissionStore>(new StoreCreator<SentTransmissionStore>()
	{
		@Override
		public SentTransmissionStore createStore() throws DBException
		{
			return new SentTransmissionStore(TransmissionClient.this);
		}
	});
	
	public final StoreHandle<ReceivedTransmissionStore> receivedTransmissionStoreHandle = new StoreHandle<ReceivedTransmissionStore>(new StoreCreator<ReceivedTransmissionStore>()
	{
		@Override
		public ReceivedTransmissionStore createStore() throws DBException
		{
			return new ReceivedTransmissionStore(TransmissionClient.this);
		}
	});
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getReserveredModels()
	 */
	@Override
	public List<Model> getReservedModels()
	{
		List<Model> reserved = super.getReservedModels();
		reserved.add(TransmissionStore.TRANSMISSION_MANAGEMENT_MODEL);
		return reserved;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getTableName(uk.ac.ucl.excites.sapelli.storage.model.Schema)
	 */
	@Override
	public String getTableName(Schema schema)
	{
		if(schema == TransmissionStore.SENT_TRANSMISSION_SCHEMA)
			return "Sent_Transmissions";
		if(schema == TransmissionStore.SENT_TRANSMISSION_PART_SCHEMA)
			return "Sent_Transmission_Parts";
		if(schema == TransmissionStore.RECEIVED_TRANSMISSION_SCHEMA)
			return "Received_Transmissions";
		if(schema == TransmissionStore.RECEIVED_TRANSMISSION_PART_SCHEMA)
			return "Received_Transmission_Parts";
		return super.getTableName(schema);
	}
	
	public abstract Payload createPayload(int nonBuiltinType);
	
	/**
	 * Returns columns from ther given schema that should not be transmitted.
	 * It is assumed these are optional columns, or (TODO once this is supported) non-optional columns with a default value.
	 * 
	 * @param schema
	 * @return
	 */
	public abstract Set<Column<?>> getNonTransmittableColumns(Schema schema);

}
