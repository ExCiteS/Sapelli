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

import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

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

	// DYNAMICS------------------------------------------------------	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getTableName(uk.ac.ucl.excites.sapelli.storage.model.Schema)
	 */
	@Override
	public String getTableName(Schema schema)
	{
		if(schema == TransmissionStore.TRANSMISSION_SCHEMA)
			return "Transmissions";
		if(schema == TransmissionStore.TRANSMISSION_PART_SCHEMA)
			return "Transmission_Parts";
		return super.getTableName(schema);
	}
	
	public abstract EncryptionSettings getEncryptionSettingsFor(Model model) throws UnknownModelException;
	
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
