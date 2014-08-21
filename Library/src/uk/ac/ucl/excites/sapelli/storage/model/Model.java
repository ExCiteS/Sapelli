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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.util.ModelFullException;

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
	
	static public int MaxSchemata()
	{
		return (int) MODEL_SCHEMA_NO_FIELD.highBound();
	}

	// Dynamics-----------------------------------------------------------
	private final long id;
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
		if(schemata.size() > MODEL_SCHEMA_NO_FIELD.highBound())
			throw new ModelFullException("The model has reached the maximum of " + MODEL_SCHEMA_NO_FIELD.highBound() + " schemata.");
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

	public Schema getSchema(int schemaNumber)
	{
		if(schemaNumber < 0 || schemaNumber > schemata.size())
			throw new IndexOutOfBoundsException("Invalid schemaNumber (" + schemaNumber + "), must be in range [0, " + (schemata.size() - 1) + "].");
		return schemata.get(schemaNumber);
	}
	
	public int getSchemaNumber(Schema schema)
	{
		int no = schemata.indexOf(schema);
		if(no == -1 || schema.getModel() != this)
			throw new IllegalArgumentException("This schema does not belong to this model");
		return no;
	}
	
	/**
	 * @return the schemata, in order of increasing schema number
	 */
	public List<Schema> getSchemata()
	{
		return new ArrayList<Schema>(schemata);
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
