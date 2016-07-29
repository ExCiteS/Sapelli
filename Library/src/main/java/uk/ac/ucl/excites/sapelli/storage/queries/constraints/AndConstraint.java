/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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
 * Conjuction of Constraints
 * 
 * @author mstevens
 */
public class AndConstraint extends CompositeConstraint
{

	public AndConstraint(Constraint... constraints)
	{
		super(constraints);
	}

	/**
	 * Applies De Morgan's laws
	 * 
	 * @see http://en.wikipedia.org/wiki/De_Morgan's_laws
	 * @see http://math.stackexchange.com/a/320689/176790
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#negate()
	 */
	@Override
	public Constraint negate()
	{
		if(!hasSubConstraints())
			return super.negate();
		OrConstraint orConstr = new OrConstraint();
		for(Constraint subConstraint : getSubConstraints())
			orConstr.addConstraint(subConstraint.negate());
		return orConstr;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		if(hasSubConstraints())
			for(Constraint subConstraint : getSubConstraints())
				if(!subConstraint._isValid(record))
					return false;
		return true; // also if we didn't have any subConstraints
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}

	@Override
	protected boolean isAssociative()
	{
		return true;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof AndConstraint)
			return super.equals(obj); // CompositeConstraint#equals()
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // CompositeConstraint#hashCode()
		hash = 31 * hash + "AND".hashCode(); // to differentiate from OrConstraint
		return hash;
	}
	
}
