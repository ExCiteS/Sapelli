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

import uk.ac.ucl.excites.sapelli.storage.model.columns.LosslessFlagColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.util.IncompletePrimaryKeyException;
import uk.ac.ucl.excites.sapelli.storage.util.InvalidValueException;

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
	 * Creates a new "empty" record of the given schema.
	 * 
	 * @param schema
	 */
	protected Record(Schema schema)
	{
		super(schema);
	}
	
	/**
	 * Creates an initialised record.
	 * 
	 * @param schema
	 * @param values to initialise record, number and types of values must match number and types of (real) columns in the schema and each value must be valid for the corresponding column (the value for an auto-incrementing primary key is allowed to be null)
	 * @throws IllegalArgumentException in case of an incorrect number of values
	 * @throws InvalidValueException in case of an invalid value
	 * @throws NullPointerException if a value is null on an non-optional column (we only allow this for an AutoIncrementingPrimaryKey column)
	 * @throws ClassCastException when a value cannot be converted/casted to the column's type {@code <T>}
	 */
	protected Record(Schema schema, Object... values) throws InvalidValueException
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
	 * Creates an initialised record.
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
	 * Creates an initialised record.
	 * 
	 * @param schema
	 * @param serialisedValues byte array to initialise ValueSet with (should not contain values of virtual columns and may or may not be {@code lossless}ly encoded, i.e. the array must be as produced by {@code #toBytes(lossless)})
	 * @param lossless whether the given byte array is a (guaranteed) lossless ({@code true}), or a (possibly) lossy ({@code false}) representation of the values
	 * @throws NullPointerException when schema is null
	 * @throws IOException when reading serialisedValues fails
	 */
	protected Record(Schema schema, byte[] serialisedValues, boolean lossless) throws NullPointerException, IOException
	{
		super(schema, serialisedValues, lossless);
	}
	
	/**
	 * Copy constructor.
	 * 
	 * @param another
	 */
	public Record(Record another)
	{
		super(another);
	}
	
	/**
	 * @return the schema
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet#getSchema()
	 */
	@Override
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
	 * Returns a reference to the record. Only works if all primary key (sub)columns have a non-null value.
	 * 
	 * @return a {@link RecordReference} instance pointing to this record
	 * @throws IncompletePrimaryKeyException if (part of) the primary key column(s) lacks a value
	 * @see uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet#getReference()
	 */
	@Override
	public RecordReference getReference() throws IncompletePrimaryKeyException
	{
		return new RecordReference(this);
	}
	
	/**
	 * Returns a {@link Constraint} that matches on the Record's primary key values.
	 * 
	 * @param allowBlanks
	 * @return a Constraint
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key (and thus covered by a recordReference) have not all been assigned a value, and {@code allowBlanks} is {@code false}
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet#getRecordQueryConstraint()
	 * @see uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet#getRecordQueryConstraint(boolean)
	 */
	@Override
	/*package*/ Constraint getRecordQueryConstraint(boolean allowBlanks) throws IncompletePrimaryKeyException
	{
		return
			new RecordReference(this, true /*allow blanks at construction time so IPKE is not thrown from here ...*/)
					.getRecordQueryConstraint(allowBlanks /*... but perhaps from here instead*/);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#isLossy()
	 */
	@Override
	public boolean isLossy()
	{
		if(!columnSet.canBeLossy())
			return false;
		else if(columnSet.containsColumn(LosslessFlagColumn.INSTANCE)) // Equivalent to: columnSet.hasFlags(StorageClient.SCHEMA_FLAG_TRACK_LOSSLESSNESS) && columnSet.canBeLossy()
			return LosslessFlagColumn.INSTANCE.isLossy(this); // more efficient and more reliable than ValueSet#isLossy()
		else
			return super.isLossy();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#isLossless()
	 */
	@Override
	public boolean isLossless()
	{
		if(!columnSet.canBeLossy())
			return true;
		else if(columnSet.containsColumn(LosslessFlagColumn.INSTANCE)) // Equivalent to: columnSet.hasFlags(StorageClient.SCHEMA_FLAG_TRACK_LOSSLESSNESS) && columnSet.canBeLossy()
			return LosslessFlagColumn.INSTANCE.isLossless(this); // more efficient and more reliable than ValueSet#isLossless()
		else
			return super.isLossless();
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
