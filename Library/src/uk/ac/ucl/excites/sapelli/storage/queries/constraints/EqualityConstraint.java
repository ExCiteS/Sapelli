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

package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Constraint that compares the values in a column using Object#equals()
 * 
 * @author mstevens
 */
public class EqualityConstraint extends Constraint
{
	
	private final ColumnPointer columnPointer;
	private final Object value;
	
	public EqualityConstraint(Column<?> column, Object value)
	{
		this(new ColumnPointer(column), value);
	}
	
	public EqualityConstraint(ColumnPointer columnPointer, Object value)
	{
		this.columnPointer = columnPointer;
		this.value = value;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		return Record.EqualValues(columnPointer.retrieveValue(record), value);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}
	
	public ColumnPointer getColumnPointer()
	{
		return columnPointer;
	}
	
	public Object getValue()
	{
		return value;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof EqualityConstraint)
		{
			EqualityConstraint that = (EqualityConstraint) obj;
			return	this.columnPointer.equals(that.columnPointer) &&
					(this.value != null ? this.value.equals(that.value) : that.value == null);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + columnPointer.hashCode();
		hash = 31 * hash + (value != null ? value.hashCode() : 0);
		return hash;
	}

}
