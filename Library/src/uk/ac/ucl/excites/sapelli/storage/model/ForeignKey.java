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
 * Class representing a foreign key, used to reference a record of another ("foreign") schema.
 * Implemented as a Record subclass, with an {@link Index} instance (the primary key of the foreign schema) as its schema.
 * 
 * @author mstevens
 */
public class ForeignKey extends Record
{
	
	static private final long serialVersionUID = 2L;
	
	private Schema foreignSchema;
	
	/**
	 * @param foreignSchema
	 * @throws NullPointerException	if the foreignSchema does not have a primary key set
	 */
	public ForeignKey(Schema foreignSchema)
	{
		 /* We use the foreign schema's primary key (an instance of Index, a subclass
		  * of Schema) as the schema for this record (ForeignKey is a subclass of Record) */
		super(foreignSchema.getPrimaryKey());
		this.foreignSchema = foreignSchema;
	}
	
	/**
	 * @param foreignRecord
	 * @throws NullPointerException	if the Schema of the foreignRecord does not have a primary key set
	 */
	public ForeignKey(Record foreignRecord)
	{
		this(foreignRecord.schema);
		
		// Copy the key part values:
		for(Column<?> keyPartCol : getIndex().getColumns(false))
		{
			Object keyPartValue = keyPartCol.retrieveValueCopy(foreignRecord);
			if(keyPartValue == null)
				throw new IllegalArgumentException("Cannot construct ForeignKey from record because key part \"" + keyPartCol.getName() + "\" has not been set");
			setValue(keyPartCol, keyPartValue);
		}
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param foreignKey
	 */
	public ForeignKey(ForeignKey foreignKey)
	{
		super(foreignKey);
		this.foreignSchema = foreignKey.foreignSchema;
	}
	
	/**
	 * @return the primary key of the foreignSchema 
	 */
	public Index getIndex()
	{
		return (Index) schema; // equivalent to: foreignSchema.getPrimaryKey()
	}
	
	/**
	 * @return the foreignSchema
	 */
	public Schema getForeignSchema()
	{
		return foreignSchema;
	}
	
	public SingleRecordQuery getForeignRecordQuery()
	{
		if(!isFilled())
			throw new IllegalStateException("All values of the key must be set before a query can be created!");
		// Match for key parts:
		AndConstraint constraints = new AndConstraint();
		int c = 0;
		for(Object keyPart : values)
			constraints.addConstraint(new EqualityConstraint(schema.getColumn(c++), keyPart));
		
		// Single record query:
		return new FirstRecordQuery(new RecordsQuery(foreignSchema, constraints));
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + foreignSchema.hashCode();
		return hash;
	}
	
}
