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
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Constraint that compares the values in a column using Object#equals()
 * 
 * @author mstevens
 */
public class JoinConstraint extends Constraint
{

	// DYNAMICS------------------------------------------------------
	private final ColumnPointer<ForeignKeyColumn> fkColumnPointer;
	private final Constraint foreignConstraints;
	
	public JoinConstraint(ForeignKeyColumn fkColumn, Constraint... foreignConstraints)
	{
		this(new ColumnPointer<ForeignKeyColumn>(fkColumn), foreignConstraints);
	}

	public JoinConstraint(ColumnPointer<ForeignKeyColumn> fkColumnPointer, Constraint... foreignConstraints)
	{
		this.fkColumnPointer = fkColumnPointer;
		this.foreignConstraints = new AndConstraint(foreignConstraints).reduce();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#negate()
	 */
	@Override
	public JoinConstraint negate()
	{
		return new JoinConstraint(fkColumnPointer, foreignConstraints != null ? foreignConstraints.negate() : null);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("JoinContraints cannot be evaluated agains single Record instances");
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}
	
	public ColumnPointer<ForeignKeyColumn> getForeignKeyColumnPointer()
	{
		return fkColumnPointer;
	}
	
	public Schema getForeignSchema()
	{
		return fkColumnPointer.getColumn().getForeignSchema();
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof JoinConstraint)
		{
			JoinConstraint that = (JoinConstraint) obj;
			return	this.fkColumnPointer.equals(that.fkColumnPointer) &&
					Objects.deepEquals(this.value, that.value);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + fkColumnPointer.hashCode();
		hash = 31 * hash + (value != null ? value.hashCode() : 0);
		return hash;
	}

}
