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

import uk.ac.ucl.excites.sapelli.storage.model.indexes.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;

/**
 * Class representing a reference to another {@link Record}, identified by the value(s) of its primary key.
 * This is equivalent to a foreign key, as used to reference a record of another ("foreign") schema.
 * 
 * Implemented as {@link RecordValueSet}, with an {@link PrimaryKey} instance (i.e. the primary key of the referenced, or "foreign" schema) as its {@link ColumnSet}.
 * 
 * @author mstevens
 */
public class RecordReference extends RecordValueSet<PrimaryKey>
{
	
	static private final long serialVersionUID = 2L;
	
	private Schema referencedSchema;
	
	/**
	 * Creates a new, but "empty", RecordReference which, once the column values have been set, can be used to reference a record of the given schema. 
	 * 
	 * @param referencedSchema (also called "foreign" schema)
	 * @throws NullPointerException	if the recordSchema does not have a primary key
	 */
	protected RecordReference(Schema referencedSchema) throws NullPointerException
	{
		this(referencedSchema, (Object []) null);
	}
	
	/**
	 * Creates a new RecordReference, to be used for referencing a record of the given schema, which is initialised with the given key part values. 
	 * 
	 * @param referencedSchema (also called "foreign" schema)
	 * @param keyPartValues to initialise the recordReference, the number of values must match number of columns in primary key of the referencedSchema
	 * @throws NullPointerException	if the referencedSchema does not have a primary key
	 */
	protected RecordReference(Schema referencedSchema, Object... keyPartValues) throws NullPointerException
	{
		super(referencedSchema.getPrimaryKey(), keyPartValues); // We use the recordSchema's primary key as the schema for this record (i.e. for the recordReference, which is "a record" in its own right)
		this.referencedSchema = referencedSchema;
	}
	
	/**
	 * Creates a new RecordReference, to be used for referencing a record of the given schema, which is initialised with the given serialised key part values.
	 * 
	 * @param referencedSchema (also called "foreign" schema)
	 * @param serialisedKeyPartValues to initialise the recordReference, the number of values must match number of columns in primary key of the referencedSchema
	 * @throws NullPointerException	if the referencedSchema does not have a primary key
	 * @throws Exception when parsing serialisedValues fails
	 */
	protected RecordReference(Schema referencedSchema, String serialisedKeyPartValues) throws NullPointerException, Exception
	{
		super(referencedSchema.getPrimaryKey(), serialisedKeyPartValues);
		this.referencedSchema = referencedSchema;
	}
	
	/**
	 * Creates a new RecordReference, to be used for referencing a record of the given schema, which is initialised with the given serialised key part values.
	 * 
	 * @param referencedSchema (also called "foreign" schema)
	 * @param serialisedKeyPartValues to initialise the recordReference, the number of values must match number of columns in primary key of the referencedSchema
	 * @throws NullPointerException	if the referencedSchema does not have a primary key
	 * @throws IOException when reading serialisedValues fails
	 */
	protected RecordReference(Schema referencedSchema, byte[] serialisedKeyPartValues) throws NullPointerException, IOException
	{
		super(referencedSchema.getPrimaryKey(), serialisedKeyPartValues);
		this.referencedSchema = referencedSchema;
	}

	/**
	 * Creates a new RecordReference which points to the given {@link Record}.
	 * 
	 * @param record the Record to be referenced ("pointed to"), also called the "foreign" record
	 * @throws NullPointerException	if the Schema of the given Record does not have a primary key
	 */
	public RecordReference(Record record) throws NullPointerException
	{
		this(record.columnSet); // !!!
		
		// Copy the key part values:
		for(Column<?> keyPartCol : columnSet.getColumns(false))
		{
			Object keyPartValue = keyPartCol.retrieveValueCopy(record);
			if(keyPartValue == null)
				throw new IllegalArgumentException("Cannot construct RecordReference from record because key part \"" + keyPartCol.getName() + "\" has not been set");
			setValue(keyPartCol, keyPartValue);
		}
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param another
	 */
	public RecordReference(RecordReference another)
	{
		super(another); // sets schema and copies values
		this.referencedSchema = another.referencedSchema;
	}
	
	/**
	 * @return the referencedSchema
	 */
	public Schema getReferencedSchema()
	{
		return referencedSchema;
	}
	
	/**
	 * Returns a {@link SingleRecordQuery} which can be used to find the referenced record in a RecordStore or Collection.
	 * 
	 * @return a query that looks for the record this reference points to
	 * @throws IllegalStateException when not all columns of this recordReference have been assigned a value
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Record#getRecordQuery()
	 * @see uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet#getRecordQuery()
	 */
	@Override
	public SingleRecordQuery getRecordQuery() throws IllegalStateException
	{
		return new FirstRecordQuery(Source.From(referencedSchema), Order.UNDEFINED, getRecordQueryConstraint());
	}
	
	/**
	 * Returns a {@link Constraint} that matches on the referenced record's primary key values.
	 * 
	 * @throws IllegalStateException when not all columns of this recordReference have been assigned a value
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Record#getRecordQueryConstraint()
	 * @see uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet#getRecordQueryConstraint()
	 */
	@Override
	public Constraint getRecordQueryConstraint() throws IllegalStateException
	{
		if(!isFilled())
			throw new IllegalStateException("All values of the key must be set before a record selecting constraint/query can be created!");
		
		// Match for key parts:
		AndConstraint constraints = new AndConstraint();
		int c = 0;
		for(Object keyPart : values)
			constraints.addConstraint(new EqualityConstraint(columnSet.getColumn(c++), keyPart));
		
		return constraints.reduce();
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + referencedSchema.hashCode();
		return hash;
	}
	
	@Override
    public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof RecordReference)
			return	super.equals(obj) &&
					this.referencedSchema.equals(((RecordReference) obj).referencedSchema);
		return false;
	}
	
}
