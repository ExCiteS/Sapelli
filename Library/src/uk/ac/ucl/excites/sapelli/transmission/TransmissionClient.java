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

import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.Index;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * @author mstevens
 *
 */
public abstract class TransmissionClient extends StorageClient
{

	// STATICS-------------------------------------------------------
	static public final IntegerColumn COLUMN_LAST_TRANSMITTED_AT = Schema.GetRawMSTimeColumn("lastTransmittedAt", true);
	
	/**
	 * @param schema a non-null, non-internal, unsealed schema
	 */
	static public void MakeTransmittable(Schema schema)
	{
		if(schema == null)
			throw new NullPointerException("schema is null");
		if(schema.isSealed())
			throw new IllegalStateException("The schema is already sealed!");
		// Add column & index:
		schema.addColumn(COLUMN_LAST_TRANSMITTED_AT);
		schema.addIndex(new Index(COLUMN_LAST_TRANSMITTED_AT, false));
	}
	
	/**
	 * @param schema
	 * @return whether or not {@link Record}s of the given {@link Schema} can be transmitted
	 */
	static public boolean isTransmittable(Schema schema)
	{
		return schema.containsColumn(COLUMN_LAST_TRANSMITTED_AT);
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
	 * ID for the reserved Transmission Management Model ({@link TransmissionStore#TRANSMISSION_MANAGEMENT_MODEL})
	 */
	static public final long TRANSMISSION_MANAGEMENT_MODEL_ID = 0;
	
	// Add tableName prefix & reserved model (in that order!):
	static
	{
		AddTableNamePrefix(SCHEMA_FLAG_TRANSMISSION_LAYER, "Transmission_");
		AddReservedModel(TransmissionStore.TRANSMISSION_MANAGEMENT_MODEL);
	}
	
	// DYNAMICS------------------------------------------------------	
	public abstract EncryptionSettings getEncryptionSettingsFor(Model model) throws UnknownModelException;
	
	public abstract Payload createPayload(int nonBuiltinType);
	
	/**
	 * Returns columns from the given schema that should not be transmitted.
	 * It is assumed these are optional columns, or non-optional columns with a default value.
	 * 
	 * Subclasses can override this but *must* return at least the same columns returned by the super implementation.
	 * 
	 * @param schema
	 * @return
	 */
	public Set<Column<?>> getNonTransmittableColumns(Schema schema)
	{
		Set<Column<?>> skipCols = new HashSet<Column<?>>();
		skipCols.add(Schema.COLUMN_LAST_EXPORTED_AT);
		skipCols.add(COLUMN_LAST_TRANSMITTED_AT);
		return skipCols;
	}
	
}
