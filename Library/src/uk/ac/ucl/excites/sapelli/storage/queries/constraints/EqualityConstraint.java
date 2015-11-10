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

import uk.ac.ucl.excites.sapelli.shared.util.Objects;
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
	
	static public EqualityConstraint IsNull(Column<?> column)
	{
		return IsNull(new ColumnPointer<Column<?>>(column));
	}

	static public EqualityConstraint IsNull(ColumnPointer<?> columnPointer)
	{
		return new EqualityConstraint(columnPointer, null);
	}
	
	static public EqualityConstraint IsNotNull(Column<?> column)
	{
		return IsNotNull(new ColumnPointer<Column<?>>(column));
	}

	static public EqualityConstraint IsNotNull(ColumnPointer<?> columnPointer)
	{
		return new EqualityConstraint(columnPointer, null, false);
	}
	
	private final ColumnPointer<?> columnPointer;
	private final Object value;
	private final boolean equal;
	
	public EqualityConstraint(Column<?> column, Object value)
	{
		this(new ColumnPointer<Column<?>>(column), value);
	}
	
	public EqualityConstraint(ColumnPointer<?> columnPointer, Object value)
	{
		this(columnPointer, value, true);
	}
	
	public EqualityConstraint(Column<?> column, Object value, boolean equal)
	{
		this(new ColumnPointer<Column<?>>(column), value, equal);
	}
	
	public EqualityConstraint(ColumnPointer<?> columnPointer, Object value, boolean equal)
	{
		this.columnPointer = columnPointer;
		this.value = value;
		this.equal = equal;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#negate()
	 */
	@Override
	public EqualityConstraint negate()
	{
		return new EqualityConstraint(	getColumnPointer(),
										getValue(),
										!equal); // invert!
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		return equal == Objects.deepEquals(columnPointer.retrieveValue(record), value);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}
	
	public ColumnPointer<?> getColumnPointer()
	{
		return columnPointer;
	}
	
	public Object getValue()
	{
		return value;
	}
	
	public boolean isEqual()
	{
		return equal;
	}
	
	public boolean isValueNull()
	{
		return value == null;
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
					Objects.deepEquals(this.value, that.value);
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
