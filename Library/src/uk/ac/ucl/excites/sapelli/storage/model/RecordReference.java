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

import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;

/**
 * Class representing a reference to another {@link Record}, identified by the value of its primary key.
 * This is equivalent to a foreign key, as used to reference a record of another ("foreign") schema.
 * 
 * Implemented as a Record subclass, with an {@link PrimaryKey} instance (i.e. the primary key of the referenced, or "foreign" schema) as its schema.
 * 
 * @author mstevens
 */
public class RecordReference extends Record
{
	
	static private final long serialVersionUID = 2L;
	
	private Schema referencedSchema;
	
	/**
	 * Creates a new, but "empty", RecordReference which, once the column values have been set, can be used to reference a record of the given schema. 
	 * 
	 * @param referencedSchema (also called "foreign" schema)
	 * @throws NullPointerException	if the recordSchema does not have a primary key
	 */
	public RecordReference(Schema referencedSchema) throws NullPointerException
	{
		super(referencedSchema.getPrimaryKey()); // We use the recordSchema's primary key as the schema for this record
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
		this(record.schema); // !!!
		
		// Copy the key part values:
		for(Column<?> keyPartCol : schema.getColumns(false))
		{
			Object keyPartValue = keyPartCol.retrieveValueCopy(record);
			if(keyPartValue == null)
				throw new IllegalArgumentException("Cannot construct ForeignKey from record because key part \"" + keyPartCol.getName() + "\" has not been set");
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
	 * Returns a {@link SingleRecordQuery} which can be used to find the reference record.
	 * 
	 * @return a query that looks for the record this reference points to
	 */
	public SingleRecordQuery getRecordQuery()
	{
		if(!isFilled())
			throw new IllegalStateException("All values of the key must be set before a query can be created!");
		
		// Match for key parts:
		AndConstraint constraints = new AndConstraint();
		int c = 0;
		for(Object keyPart : values)
			constraints.addConstraint(new EqualityConstraint(schema.getColumn(c++), keyPart));
		
		// Single record query:
		return new FirstRecordQuery(new RecordsQuery(referencedSchema, constraints));
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
