/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * A Constraint that either accepts all or refuses all records.
 * 
 * @author mstevens
 */
public class DummyConstraint extends Constraint
{

		
	static public final DummyConstraint ACCEPT_ALL = new DummyConstraint(true);
	
	static public final DummyConstraint ACCEPT_NONE = new DummyConstraint(false);
	
	public final boolean allValid;
	
	private DummyConstraint(boolean allValid)
	{
		this.allValid = allValid;
	}

	@Override
	protected boolean _isValid(Record record)
	{
		return allValid;
	}

	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#reduce()
	 */
	@Override
	public Constraint reduce()
	{
		return allValid ? null : this;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#negate()
	 */
	@Override
	public Constraint negate()
	{
		return new DummyConstraint(!allValid).reduce();
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof DummyConstraint)
			return this.allValid == ((DummyConstraint) obj).allValid;
		return false;
	}

	@Override
	public int hashCode()
	{
		int hash = getClass().getSimpleName().hashCode();
		hash = 31 * hash + Boolean.hashCode(allValid);
		return hash;
	}

}
