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

import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;
import uk.ac.ucl.excites.sapelli.storage.util.IncompletePrimaryKeyException;

/**
 * A class representing records of a certain Schema
 * 
 * @author mstevens
 */
public class Record extends RecordValueSet<Schema>
{
	
	// Statics-------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static public final String TAG_RECORD = "Record";
	
	// Dynamics------------------------------------------------------
	
	/**
	 * Creates a new "empty" record of the given schema
	 * 
	 * @param schema
	 */
	protected Record(Schema schema)
	{
		super(schema);
	}
	
	/**
	 * Creates an initialised record
	 * 
	 * @param schema
	 * @param values to initialise record, number and types of values must match number and types of (real) columns in the schema and each value must be valid for the corresponding column (the value for an auto-incrementing primary key is allowed to be null)
	 */
	protected Record(Schema schema, Object... values)
	{
		super(schema);
		if(values != null)
		{
			Column<?> autoKeyCol = columnSet.getAutoIncrementingPrimaryKeyColumn();
			if(this.values.length == values.length)
			{
				// Init from given values:
				for(int c = 0; c < this.values.length; c++)
				{
					Column<?> col = columnSet.getColumn(c);
					if(col == autoKeyCol && values[c] == null)
						autoKeyCol.clearValue(this); // AutoKey value has not been set yet (we allow this)
					else
						col.storeObject(this, values[c]); // validation (and possibly conversion) will be applied
				}
			}
			else
				throw new IllegalArgumentException("Unexpected number of values (given: " + values.length + "; expected: " + this.values.length + ").");
		}
	}
	
	/**
	 * Creates an initialised record
	 * 
	 * @param schema
	 * @param serialisedValues String to initialise record (should not contain values of virtual columns, i.e. the String must be as produced by {@link #serialise()})
	 * @throws Exception 
	 */
	protected Record(Schema schema, String serialisedValues) throws Exception
	{
		super(schema, serialisedValues);
	}

	/**
	 * Creates an initialised record
	 * 
	 * @param schema
	 * @param serialisedValues byte array to initialise record (should not contain values of virtual columns, i.e. the String must be as produced by {@link #toBytes()})
	 * @throws NullPointerException when schema is null
	 * @throws IOException when reading serialisedValues fails
	 */
	protected Record(Schema schema, byte[] serialisedValues) throws NullPointerException, IOException
	{
		super(schema, serialisedValues);
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param another
	 */
	public Record(Record another)
	{
		super(another);
	}
	
	/**
	 * @return the schema
	 */
	public Schema getSchema()
	{
		return columnSet;
	}
	
	/**
	 * Override the schema object with another one, if compatible
	 * 
	 * @param newSchema
	 * @throws IllegalArgumentException - when the new schema is incompatible with the old one
	 * 
	 * @deprecated unsafe, avoid this unless there is a very good reason and you know what you are doing
	 */
	public void setSchema(Schema newSchema) throws IllegalArgumentException
	{
		setSchema(newSchema, false);
	}
	
	/**
	 * Override the schema object with another one, if compatible (unless forced)
	 * 
	 * @param newSchema
	 * @param force - if true the old and new schema are *not* compared, but the number of columns of the new schema must *always* match the number of values!
	 * @throws IndexOutOfBoundsException - when the new schema has a different number of columns than the number of values in the record
	 * @throws IllegalArgumentException - when the new schema is incompatible with the old one
	 * 
	 * @deprecated unsafe, avoid this unless there is a very good reason and you know what you are doing
	 */
	public void setSchema(Schema newSchema, boolean force) throws IndexOutOfBoundsException, IllegalArgumentException
	{
		if(force)
		{		
			if(newSchema.getNumberOfColumns(false) != values.length)
				throw new IndexOutOfBoundsException("The new schema has a different number of columns than the number of values in this record!");
		}
		else
		{
			if(!columnSet.equals(newSchema, true, true, false)) // also checks columns, but not indexes
				throw new IllegalArgumentException("The provived schema is not compatible with this record!");
		}
		this.columnSet = newSchema; // we accept the new one
	}
	
	/**
	 * Returns a reference to the record. Only works if the schema has a primary key.
	 * 
	 * @return a {@link RecordReference} instance pointing to this record
	 * @throws NullPointerException	if the Schema of this Record does not have a primary key
	 * @throws IncompletePrimaryKeyException if (part of) the primary key column(s) lacks a value
	 */
	@Override
	public RecordReference getReference() throws NullPointerException, IncompletePrimaryKeyException
	{
		return new RecordReference(this);
	}
	
	/**
	 * Returns a {@link SingleRecordQuery} which can be used to find this record in a RecordStore or Collection, by matching the primary key (and no other columns!).
	 * 
	 * @return a query that looks for this record
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key have not all been assigned a value
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet#getRecordQuery()
	 */
	@Override
	public SingleRecordQuery getRecordQuery() throws IncompletePrimaryKeyException
	{
		return new FirstRecordQuery(Source.From(columnSet), Order.UNDEFINED, getRecordQueryConstraint());
	}
	
	/**
	 * Returns a {@link Constraint} that matches on the Record's primary key values.
	 * 
	 * @return a Constraint
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key have not all been assigned a value
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet#getRecordQueryConstraint()
	 */
	@Override
	public Constraint getRecordQueryConstraint() throws IncompletePrimaryKeyException
	{
		if(!isFilled(columnSet.getPrimaryKey()))
			throw new IncompletePrimaryKeyException("All values of the key must be set before a record selecting contraint/query can be created!");
		
		// Match for key parts:
		AndConstraint constraints = new AndConstraint();
		for(Column<?> keyPartCol : columnSet.getPrimaryKey().getColumns(false))
			constraints.addConstraint(new EqualityConstraint(keyPartCol, getValue(keyPartCol)));
		
		return constraints.reduce();
	}
	
	/**
	 * @param obj
	 * @param checkSchema
	 * @param asStoredBinary whether or not to compare values as if they've been written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean equals(Object obj, boolean checkSchema, boolean asStoredBinary)
	{
		if(this == obj)
			return true;
		return obj instanceof Record && super.equals(obj, checkSchema, asStoredBinary);
	}
	
}
