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

package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A composite column holding columns that store the parts of a foreign key
 * 
 * @author mstevens
 */
public class ForeignKeyColumn extends RecordColumn<RecordReference>
{
	
	static private final long serialVersionUID = 2L;

	private Schema foreignSchema;
	
	/**
	 * @param foreignSchema
	 * @param optional
	 * @throws NullPointerException	if the foreignSchema does not have a primary key set
	 */
	public ForeignKeyColumn(Schema foreignSchema, boolean optional)
	{
		this(foreignSchema.getName(), foreignSchema, optional);
	}

	/**
	 * @param name
	 * @param foreignSchema
	 * @param optional
	 * @throws NullPointerException	if the foreignSchema does not have a primary key set
	 */
	public ForeignKeyColumn(String name, Schema foreignSchema, boolean optional)
	{
		super(RecordReference.class, name, foreignSchema.getPrimaryKey() /* Index instance, a subclass of Schema */, optional);
		this.foreignSchema = foreignSchema;
	}
	
	@Override
	public ForeignKeyColumn copy()
	{
		return new ForeignKeyColumn(name, foreignSchema, optional);
	}

	@Override
	public RecordReference getNewRecord()
	{
		return new RecordReference(foreignSchema);
	}

	@Override
	protected RecordReference copy(RecordReference value)
	{
		return new RecordReference(value);
	}
	
	@Override
	public void accept(ColumnVisitor visitor)
	{
		if(visitor.allowForeignKeySelfTraversal())
			super.accept(visitor, true);
		else
			visitor.visit(this);
	}
	
	/**
	 * Even though the type is actually RecordReference we call this column a "ForeignKeyColumn" 
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#getTypeString()
	 */
	@Override
	public String getTypeString()
	{
		return "ForeignKey";
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + foreignSchema.hashCode();
		return hash;
	}

}
