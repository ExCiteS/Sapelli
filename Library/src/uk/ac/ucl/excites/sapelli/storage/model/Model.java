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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.model.Schema.InternalKind;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ModelFullException;
import uk.ac.ucl.excites.sapelli.transmission.compression.LZMACompressor;

/**
 * A Model groups a series of {@link Schema}s that belong together.
 * 
 * @author mstevens
 */
public class Model implements Serializable
{
	
	// Statics------------------------------------------------------------
	private static final long serialVersionUID = 2L;
	
	/* 	The Model ID identifies the data model a schema is part of.
	 * 	The concept of the data model is left to defined by client code using the storage layer.
	 *  This is only relevant for "external"/"client" schema instances because "internal"
	 * 	schemata never belong to a model directly.
	 * 	In the case of the Sapelli Collector the data model is a Project and the model ID corresponds
	 * 	to the combination of the project ID (= 24 bits) and the project fingerprint (= 32 bits), which are
	 *  combined into one 56 bit value in SapelliCollectorClient. */
	static public final int MODEL_ID_SIZE = 56; // bits
	/**
	 * Model ID: unsigned(!) 56 bit integer
	 */
	static public final IntegerRangeMapping MODEL_ID_FIELD = IntegerRangeMapping.ForSize(0, MODEL_ID_SIZE);
	
	/*	the Model Schema Number signifies the number, position or index of a schema within the model it belongs to.
	 * 	In the case of the Sapelli Collector it the value corresponds to the position within the project of the 
	 * 	form the schema corresponds to. Model schema numbers start at 0, meaning that the first schema added to
	 * a model gets schema number 0 (not 1). */
	static public final int MODEL_SCHEMA_NO_SIZE = 4; // bits
	/**
	 * Model schema number: unsigned(!) 4 bit integer
	 */
	static public final IntegerRangeMapping MODEL_SCHEMA_NO_FIELD = IntegerRangeMapping.ForSize(0, MODEL_SCHEMA_NO_SIZE); // [0, 15]
	
	static public final int MAX_SCHEMATA = MODEL_SCHEMA_NO_FIELD.numberOfPossibleValues().intValue(); // = 16
	
	// Model Schema: a "meta" schema for records that describe a Model
	static public final Schema MODEL_SCHEMA = new Schema(InternalKind.MODEL);
	static public final IntegerColumn MODEL_ID_COLUMN = new IntegerColumn("ID", false, Model.MODEL_ID_FIELD);
	static private final StringColumn MODEL_NAME_COLUMN = StringColumn.ForCharacterCount("name", false, 128);
	static private final ByteArrayColumn MODEL_OBJECT_SERIALISATION_COLUMN = new ByteArrayColumn("serialisedObject_LZMA", false);
	static private final IntegerColumn MODEL_OBJECT_HASHCODE_COLUMN = new IntegerColumn("hashCode", false, true, Integer.SIZE);
	static
	{
		MODEL_SCHEMA.addColumn(MODEL_ID_COLUMN);
		MODEL_SCHEMA.addColumn(MODEL_NAME_COLUMN);
		MODEL_SCHEMA.addColumn(MODEL_OBJECT_SERIALISATION_COLUMN);
		MODEL_SCHEMA.addColumn(MODEL_OBJECT_HASHCODE_COLUMN);
		MODEL_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(MODEL_ID_COLUMN));
		MODEL_SCHEMA.seal();
	}
	
	/**
	 * Returns "model record" which describes the given model (and contains a serialised version of it)
	 * 
	 * @param schema
	 * @return
	 * @throws IOException
	 */
	static public Record GetModelRecord(Model model) throws IOException
	{
		// Serialise Model object:
		ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
		ObjectOutputStream objOut = new ObjectOutputStream(rawOut);
		objOut.writeObject(model);
		objOut.flush();
		objOut.close();
		byte[] lzmaObjectBytes = new LZMACompressor().compress(rawOut.toByteArray());
		
		return MODEL_SCHEMA.createRecord(model.id, model.name, lzmaObjectBytes, model.hashCode());
	}
	
	/**
	 * Returns a RecordReference pointing to a (hypothetical) model record, describing the given model,
	 * but avoids actually instantiating the whole model record itself. 
	 * 
	 * @param model
	 * @return
	 */
	static public RecordReference GetModelRecordReference(Model model)
	{
		return new RecordReference(MODEL_SCHEMA, model.id);
	}
	
	/**
	 * Returns a RecordReference pointing to a (hypothetical) model record, describing a model with the given ID.
	 * 
	 * @param modelID
	 * @return
	 */
	static public RecordReference GetModelRecordReference(long modelID)
	{
		return new RecordReference(MODEL_SCHEMA, modelID);
	}
	
	static public Model FromModelRecord(Record modelRecord) throws NullPointerException, IllegalArgumentException, IOException, ClassNotFoundException
	{
		if(modelRecord == null)
			throw new NullPointerException("The modelRecord cannot be null!");
		if(modelRecord.getSchema() != MODEL_SCHEMA)
			throw new IllegalArgumentException("The given record is a not a " + MODEL_SCHEMA.name + " record!");
		
		// Decompress & deserialise Schema object bytes:
		ByteArrayInputStream rawIn = new ByteArrayInputStream(new LZMACompressor().decompress(MODEL_OBJECT_SERIALISATION_COLUMN.retrieveValue(modelRecord)));
		ObjectInputStream objIn = new ObjectInputStream(rawIn);
		Model model = (Model) objIn.readObject();
		
		// Perform check:
		if(model.hashCode() != MODEL_OBJECT_HASHCODE_COLUMN.retrieveValue(modelRecord))
			throw new IllegalStateException("Model hashCode mismatch");
		// Note: if hashCode matches then id, name should match as well
		
		return model;
	}

	// Dynamics-----------------------------------------------------------
	public final long id;
	private final String name;
	private final List<Schema> schemata = new ArrayList<Schema>();
	private boolean sealed = false;
	
	/**
	 * Creates a new model
	 * 
	 * @param id
	 * @param name
	 */
	public Model(long id, String name)
	{
		if(!MODEL_ID_FIELD.fits(id))
			throw new IllegalArgumentException("Model ID is not valid, must be from range " + MODEL_ID_FIELD.getLogicalRangeString() + ".");
		if(name == null || name.isEmpty())
			throw new NullPointerException("Please provide a model name");
		this.id = id;
		this.name = name;
	}
	
	/**
	 * @return the id
	 */
	public long getID()
	{
		return id;
	}

	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}
	
	/**
	 * Adds a given schema to the model (provided it is not sealed, nor full)
	 * 
	 * @param schema the schema to add
	 * @return the schema number which has been assigned to the added schema
	 * @throws ModelFullException
	 */
	protected int addSchema(Schema schema) throws ModelFullException
	{
		if(sealed)
			throw new IllegalStateException("Cannot extend sealed model!");
		if(schema == null)
			throw new NullPointerException("Cannot add null Schema");
		if(schemata.size() == MAX_SCHEMATA)
			throw new ModelFullException("The model has reached the maximum of " + MAX_SCHEMATA + " schemata.");
		schemata.add(schema);
		return schemata.size() - 1;
	}
	
	/**
	 * @return the sealed
	 */
	public boolean isSealed()
	{
		return sealed;
	}

	/**
	 * seals the schema, after which records can be created based on it, but no more columns can be added
	 */
	public void seal()
	{
		this.sealed = true;
	}

	/**
	 * @param schemaNumber
	 * @return
	 */
	public Schema getSchema(int schemaNumber)
	{
		if(schemaNumber < 0 || schemaNumber > schemata.size())
			throw new IndexOutOfBoundsException("Invalid schemaNumber (" + schemaNumber + "), must be in range [0, " + (schemata.size() - 1) + "].");
		return schemata.get(schemaNumber);
	}
	
	/**
	 * @param schema
	 * @return
	 */
	public int getSchemaNumber(Schema schema)
	{
		if(schema.getModel() != this)
			throw new IllegalArgumentException("This schema does not belong to this model");
		int no = schemata.indexOf(schema);
		if(no == -1 || schema.getModel() != this)
			throw new IllegalArgumentException("This schema does not belong to this model"); // shouldn't happen
		return no;
	}
	
	/**
	 * @return the schemata, in order of increasing schema number
	 */
	public List<Schema> getSchemata()
	{
		return Collections.unmodifiableList(schemata);
	}

	public int getNumberOfSchemata()
	{
		return schemata.size();
	}
	
	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (int)(id ^ (id >>> 32));
		hash = 31 * hash + (name == null ? 0 : name.hashCode());
		hash = 31 * hash + schemata.hashCode();
		hash = 31 * hash + (sealed ? 0 : 1);
		return hash;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj) // compare pointers first
			return true;
		if(obj instanceof Model)
		{
			Model that = (Model) obj;
			return	this.id == that.id &&
					this.name.equals(that.name) &&
					this.schemata.equals(that.schemata) &&
					this.sealed == that.sealed;
		}
		return false;
	}
	
}
