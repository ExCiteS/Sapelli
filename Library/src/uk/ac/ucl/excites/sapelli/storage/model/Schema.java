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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.Index;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.util.ModelFullException;

/**
 * A Schema holds a set of ordered {@link Column}s
 * 
 * @author mstevens
 */
public class Schema extends ColumnSet implements Serializable
{

	// STATICS ----------------------------------------------------------------
	private static final long serialVersionUID = 2L;
	
	static public final String COLUMN_AUTO_KEY_NAME = "AutoKey";
	
	// XML attributes (for record exports):
	static public final String ATTRIBUTE_MODEL_ID = "modelID";
	static public final String ATTRIBUTE_MODEL_SCHEMA_NUMBER = "modelSchemaNumber";
	static public final String ATTRIBUTE_SCHEMA_NAME = "schemaName";
	
	// v1.x-style identification (for backwards compatibility only):
	//	Note: schemaID & schemaVersion are no longer stored in a Schema instance, instead a 1.x Project instance holds them (Project#id = schemaID & Project#schemaVersion = schemaVersion) 
	static public final int V1X_SCHEMA_ID_SIZE = 24; // bits
	static public final int V1X_SCHEMA_VERSION_SIZE = 8; // bits
	static public final IntegerRangeMapping V1X_SCHEMA_VERSION_FIELD = IntegerRangeMapping.ForSize(0, V1X_SCHEMA_VERSION_SIZE); // unsigned 24 bit integer
	static public final int V1X_DEFAULT_SCHEMA_VERSION = 0;
	// 	Note the XML attributes below have inconsistent naming (for everything else we've been using CamelCase instead of dashes), won't fix because no longer used in v2.x
	static public final String V1X_ATTRIBUTE_SCHEMA_ID = "schema-id";
	static public final String V1X_ATTRIBUTE_SCHEMA_VERSION = "schema-version";
	
	static public final int MAX_SCHEMA_NAME_LENGTH = 256; // chars
	
	/**
	 * Returns a "meta" record which describes the given schema.
	 * 
	 * @param schema
	 * @return
	 */
	static public Record GetMetaRecord(Schema schema)
	{
		return Model.SCHEMA_SCHEMA.createRecord(Model.GetModelRecordReference(schema.model), schema.modelSchemaNumber, schema.name, schema.flags, schema.tableName);
	}
	
	/**
	 * Returns a RecordReference pointing to a (hypothetical) meta record of the given schema,
	 * but avoids actually instantiating the whole meta record itself. 
	 * 
	 * @param schema
	 * @return
	 */
	static public RecordReference GetMetaRecordReference(Schema schema)
	{
		return new RecordReference(Model.SCHEMA_SCHEMA, Model.GetModelRecordReference(schema.model), schema.modelSchemaNumber);
	}
	
	/**
	 * Column to keep track of when a record was last stored/updates in the/a RecordStore,
	 * only used on Schemata which have the {@link StorageClient#SCHEMA_FLAG_TRACK_CHANGES} flag set.
	 * Careful: only RecordStores, record import classes, and the RecordsPayload class should write to this column.
	 */
	static public final IntegerColumn COLUMN_LAST_STORED_AT = new IntegerColumn("lastStoredAt", false, true, Long.SIZE);
	
	/**
	 * Column to keep track of when a record was last exported,
	 * only used on Schemata which have the {@link StorageClient#SCHEMA_FLAG_EXPORTABLE} flag set.
	 * Careful: only RecordStores should write to this column.
	 */
	static public final IntegerColumn COLUMN_LAST_EXPORTED_AT = new IntegerColumn("lastExportedAt", true, true, Long.SIZE);
	
	// DYNAMICS ---------------------------------------------------------------
	public final String tableName;
	public final Model model;
	public final int modelSchemaNumber;
	public final int flags;
	
	private PrimaryKey primaryKey;
	
	/**
	 * List of indexes, also includes the primary key
	 */
	private List<Index> indexes;
	
	/**
	 * Create a new schema instance which will be add to the provided {@link Model}.
	 * The Schema will use the default schema flags of the Model, if the model does not have
	 * default schema flags a NullPointerException will be thrown.
	 * 
	 * @param model
	 * @param name (will also be used as tableName)
	 * @throws ModelFullException if the model is full
	 * @throws NullPointerException
	 */
	public Schema(Model model, String name) throws ModelFullException, NullPointerException
	{
		this(model, name, name);
	}

	/**
	 * Create a new schema instance which will be add to the provided {@link Model}.
	 * The Schema will use the default schema flags of the Model, if the model does not have default schema flags a NullPointerException will be thrown.
	 * 
	 * @param model
	 * @param name
	 * @param tableName alternative name to use when recreating database table for storing Records of this Schema (e.g. the plural form of the name)
	 * @throws ModelFullException if the model is full
	 * @throws NullPointerException
	 */
	public Schema(Model model, String name, String tableName) throws ModelFullException, NullPointerException
	{
		this(model, name, tableName, model.getDefaultSchemaFlags());
	}
	
	/**
	 * Create a new schema instance which will be add to the provided {@link Model}.
	 * 
	 * @param model
	 * @param name (will also be used as tableName)
	 * @param flags
	 * @throws ModelFullException if the model is full
	 * @throws NullPointerException
	 */
	public Schema(Model model, String name, int flags) throws ModelFullException, NullPointerException
	{
		this(model, name, name, flags);
	}
	
	/**
	 * Create a new schema instance which will be add to the provided {@link Model}.
	 * 
	 * @param model
	 * @param name
	 * @param tableName alternative name to use when recreating database table for storing Records of this Schema (e.g. the plural form of the name)
	 * @param flags
	 * @throws ModelFullException if the model is full
	 * @throws NullPointerException
	 */
	public Schema(Model model, String name, String tableName, int flags) throws ModelFullException, NullPointerException
	{
		super((name == null || name.trim().isEmpty() ? model.getName() + "_Schema" + model.getNumberOfSchemata() : name), true);
		if(this.name.length() > MAX_SCHEMA_NAME_LENGTH)
			throw new IllegalArgumentException("Please provide a schema name of maximum " + MAX_SCHEMA_NAME_LENGTH + " characters");
		if(model == null)
			throw new NullPointerException("Please specify an non-null Model");
		this.tableName = tableName != null ? tableName : this.name;
		this.model = model;
		this.modelSchemaNumber = model.addSchema(this); // add oneself to the model!
		this.flags = flags;
	}
	
	/**
	 * @return the model
	 */
	public Model getModel()
	{
		return model;
	}
	
	/**
	 * @return the modelID (unsigned 56 bit integer)
	 */
	public long getModelID()
	{
		return getModel().getID();
	}

	/**
	 * @return the modelSchemaNo (unsigned 4 bit integer)
	 */
	public int getModelSchemaNumber()
	{
		return modelSchemaNumber;
	}
	
	/**
	 * @return the tableName
	 */
	protected final String getTableName()
	{
		return tableName;
	}

	/**
	 * @return the flags
	 */
	public int getFlags()
	{
		return flags;
	}

	/**
	 * Check whether the Schema has the given flags enabled.
	 * 
	 * @param flags
	 * @return
	 */
	public boolean hasFlags(int flags)
	{
		return StorageClient.TestSchemaFlags(this.flags, flags);
	}
	
	/**
	 * Returns a "meta" record which describes the schema.
	 * 
	 * @return meta Record
	 * @see #GetMetaRecord(Schema)
	 */
	public Record getMetaRecord()
	{
		return GetMetaRecord(this);
	}
	
	/**
	 * Returns a RecordReference pointing to a (hypothetical) meta record of given schema,
	 * but avoids actually instantiating the whole meta record itself. 
	 * 
	 * @return meta RecordReference
	 * @see #GetMetaRecordReference(Schema)
	 */
	public RecordReference getMetaRecordReference()
	{
		return GetMetaRecordReference(this);
	}
	
	/**
	 * Add an {@link Index} to the Schema. If the Index is a primary key it is added as such (provided does not already have a primary key).
	 * Adding indexes is possible after the Schema has been sealed (setting/changing the primary key is not).
	 * 
	 * @param index
	 * @return the added index
	 */
	public <I extends Index> I addIndex(I index)
	{
		if(index instanceof PrimaryKey)
			// set as primary key:
			setPrimaryKey((PrimaryKey) index);
		else
			// add as normal index:
			doAddIndex(index);
		return index;
	}
	
	/**
	 * @param primaryKey
	 */
	public void setPrimaryKey(PrimaryKey primaryKey)
	{
		setPrimaryKey(primaryKey, false);
	}
	
	/**
	 * @param primaryKey
	 * @param seal if {@code true} the Schema will be sealed after setting the primary key
	 */
	public void setPrimaryKey(PrimaryKey primaryKey, boolean seal)
	{
		if(this.primaryKey != null)
			throw new IllegalStateException("This Schema already has a primary key (there can be only 1)!");
		if(isSealed())
			throw new IllegalStateException("Cannot set primary key on a sealed schema!");
		// Also add as an index (+ do checks):
		doAddIndex(primaryKey);
		// Set primary key:
		this.primaryKey = primaryKey;
		// Seal if needed:
		if(seal)
			seal();
	}

	private void doAddIndex(Index index)
	{
		// Null check:
		if(index == null)
			throw new NullPointerException("Index cannot be null!");
		// Check if the indexed columns are columns of this Schema instance:
		for(Column<?> idxCol : index.getColumns(false))
		{
			// Check if idxCol is known:
			if(!containsColumn(idxCol)) 
				throw new IllegalArgumentException("Indexed column '" + idxCol.getName() + "' does not belong to this Schema. Indexed columns need to be added to the Schema before indexes are added or the primary key is set.");
		}
		// Initialise collection if needed:
		if(indexes == null)
			indexes = new ArrayList<Index>();
		// Add to the indexes:
		indexes.add(index);
	}
	
	@Override
	protected void sealTasks()
	{
		// Add automatic primary key to Schemata that don't have one yet:
		if(!hasPrimaryKey())
		{
			IntegerColumn autoKeyCol = new IntegerColumn(COLUMN_AUTO_KEY_NAME, false, true, Long.SIZE); // signed 64 bit, based on ROWIDs in SQLite v3 and later (http://www.sqlite.org/version3.html)
			this.addColumn(autoKeyCol, false /*no virtual versions to consider*/, false /*avoid endless sealing loop!*/);
			setPrimaryKey(new AutoIncrementingPrimaryKey(name + "_Idx" + COLUMN_AUTO_KEY_NAME, autoKeyCol));
		}
		// Add lastStoredAt & lastExportedAt:
		if(hasFlags(StorageClient.SCHEMA_FLAG_TRACK_CHANGES))
			this.addColumn(COLUMN_LAST_STORED_AT, false /*no virtual versions to consider*/, false /*avoid endless sealing loop!*/);
		if(hasFlags(StorageClient.SCHEMA_FLAG_EXPORTABLE))
			this.addColumn(COLUMN_LAST_EXPORTED_AT, false /*no virtual versions to consider*/, false /*avoid endless sealing loop!*/);
	}
		
	/**
	 * Returns all indexes (if there are any), including the primary key (if one is set).
	 * 
	 * @return the indexes
	 */
	public List<Index> getIndexes()
	{
		return getIndexes(true);
	}

	/**
	 * Returns a list of all indexes, including the primary key if {@code includePrimaryKey} is {@code true}. 
	 * 
	 * @param includePrimaryKey
	 * @return an unmodifiable list of indexes
	 */
	public List<Index> getIndexes(boolean includePrimaryKey)
	{
		if(indexes == null)
			// No indexes (yet):
			return Collections.<Index> emptyList();
		if(includePrimaryKey)
			// including PK:
			return Collections.unmodifiableList(indexes);
		// Excluding PK:
		List<Index> indexesWithoutPK = new ArrayList<Index>();
		for(Index idx : indexes)
			if(idx != primaryKey)
				indexesWithoutPK.add(idx);
		return Collections.unmodifiableList(indexesWithoutPK);
	}

	/**
	 * @return the primaryKey
	 */
	public PrimaryKey getPrimaryKey()
	{
		return primaryKey;
	}
	
	/**
	 * @return	whether or not the Schema has a primary key
	 */
	public boolean hasPrimaryKey()
	{
		return primaryKey != null;
	}
	
	/**
	 * @return the integerColumn of the AutoIncrementingPrimaryKey, or null if there is no primary key or it is not an AutoIncrementingPrimaryKey
	 */
	public IntegerColumn getAutoIncrementingPrimaryKeyColumn()
	{
		if(primaryKey instanceof AutoIncrementingPrimaryKey)
			return ((AutoIncrementingPrimaryKey) primaryKey).getColumn();
		else
			return null;
	}
	
	/**
	 * Finds the (first, and presumed only) index on/containing the given column. If the column is not indexed {@code null} is returned.
	 * 
	 * @param column
	 * @return the (first) index on the given column, or null if it is not indexed
	 */
	public Index getIndex(Column<?> column)
	{
		for(Index idx : getIndexes())
			if(idx.containsColumn(column))
				return idx;
		return null;
	}
	
	/**
	 * @return
	 */
	public Record createRecord()
	{
		return new Record(this);
	}
	
	/**
	 * @param values to initialise record, number and types of values must match number and types of (real) columns in the schema and each value must be valid for the corresponding column
	 * @return
	 * 
	 * @see Record#Record(Schema, Object...)
	 */
	public Record createRecord(Object... values)
	{
		return new Record(this, values);
	}
	
	/**
	 * @param serialisedValues without virtual columns!
	 * @return
	 * @throws Exception 
	 */
	public Record createRecord(String serialisedValues) throws Exception
	{
		return new Record(this, serialisedValues);
	}
	
	/**
	 * @param serialisedValues without virtual columns!
	 * @return
	 * @throws Exception 
	 */
	public Record createRecord(byte[] serialisedValues) throws IOException
	{
		return new Record(this, serialisedValues);
	}
	
	/**
	 * Create an uninitialised reference to a record of this schema
	 * 
	 * @return
	 */
	public RecordReference createRecordReference()
	{
		return new RecordReference(this);
	}
	
	/**
	 * Create an initialised reference to a record of this schema
	 * 
	 * @param keyPartValues
	 * @return
	 */
	public RecordReference createRecordReference(Object... keyPartValues)
	{
		return new RecordReference(this, keyPartValues);
	}
	
	/**
	 * Create an initialised reference to a record of this schema
	 * 
	 * @param serialiseKeyPartValues
	 * @return
	 * @throws Exception 
	 * @throws NullPointerException 
	 */
	public RecordReference createRecordReference(String serialisedKeyPartValues) throws NullPointerException, Exception
	{
		return new RecordReference(this, serialisedKeyPartValues);
	}
	
	/**
	 * Create an initialised reference to a record of this schema
	 * 
	 * @param serialiseKeyPartValues
	 * @return
	 * @throws NullPointerException
	 * @throws IOException 
	 */
	public RecordReference createRecordReference(byte[] serialisedKeyPartValues) throws NullPointerException, IOException
	{
		return new RecordReference(this, serialisedKeyPartValues);
	}
	
	/**
	 * Check for equality.
	 * 
	 * @param obj object to compare this one with
	 * @return whether or not the given Object is a Schema with the same ID & version as this one
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		return equals(obj, true, true, true);
	}

	/**
	 * Check if the provided object is an identical/equivalent Schema. The usageID & usageSubID are always checked, names and columns are optionally checked, descriptions are ignored. 
	 * 
	 * @param obj object to compare this one with
	 * @param checkNames whether or not to compare the names of the schemas and (if checkColumns is true) those of their columns
	 * @param checkColumns whether or not to compare columns (types, sizes, etc., and names if checkNames is true)
	 * @param checkIndexes whether or not to compare indexes
	 * @return whether or not the given Object is an identical/equivalent Schema (under the given checking conditions)
	 */
	public boolean equals(Object obj, boolean checkNames, boolean checkColumns, boolean checkIndexes)
	{
		if(this == obj) // compare pointers first
			return true;
		if(obj instanceof Schema)
		{
			Schema that = (Schema) obj;
			// Compare as ColumnSets:
			if(!super.equals(that, checkNames, checkColumns))
				return false;
			// Check tableName:
			if(checkNames && !this.tableName.equals(that.tableName))
				return false;
			// Check Model id & the schema number of this schema within the model:
			if(this.model.getID() != that.model.getID() || this.modelSchemaNumber != that.modelSchemaNumber)			
				return false;
			// Check flags:
			if(this.flags != that.flags)
				return false;
			// Model name (schema is name already checked at ColumnSet level):
			if(checkNames && !this.model.getName().equals(that.model.getName()))
				return false;
			// Columns are already checked at ColumnSet level
			// Check indexes:
			if(checkIndexes && !Objects.equals(this.getIndexes(), that.getIndexes())) // also checks primary key
				return false;
			// Fully equal:
			return true;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + tableName.hashCode();
		hash = 31 * hash + ((int) (model.getID() ^ (model.getID() >>> 32))); // do not use model.hashCode() here!
		hash = 31 * hash + modelSchemaNumber;
		hash = 31 * hash + getIndexes().hashCode(); // contains primary key
		hash = 31 * hash + flags;
		return hash;
	}
	
	public String getSpecification()
	{
		StringBuffer bff = new StringBuffer();
		bff.append(toString() + ":");
		for(Column<?> c : getColumns(true))
			bff.append("\n\t- " + c.getSpecification());
		// TODO add indexes & primary key to schema specs
		return bff.toString();
	}
	
	/**
	 * For sorting Schema collections
	 * 
	 * @author mstevens
	 */
	static public class Comparator implements java.util.Comparator<Schema> 
	{
		
		/**
		 * @param Schema
		 * @return unsigned 60 bit integer
		 */
		public long getSortCode(Schema schema)
		{
			return	(schema.getModelID() << Model.MODEL_SCHEMA_NO_SIZE) +	// Model ID takes up first 56 bits
					schema.modelSchemaNumber;								// Model Schema Number takes up next 4 bits
		}

		@Override
		public int compare(Schema s1, Schema s2)
		{
			return Long.compare(getSortCode(s1), getSortCode(s2));
		}
		
	}
	
}
