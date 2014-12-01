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

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Disjunction of Constraints.
 * 
 * @author mstevens
 */
public class OrConstraint extends CompositeConstraint
{

	public OrConstraint(Constraint... constraints)
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
		AndConstraint andConstr = new AndConstraint();
		for(Constraint subConstraint : getSubConstraints())
			andConstr.addConstraint(subConstraint.negate());
		return andConstr;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		if(!hasSubConstraints())
			return true; // if we do not have subConstraints then any record is valid.
		for(Constraint subConstraint : getSubConstraints())
			if(subConstraint._isValid(record))
				return true;
		return false; // if we *do* have subconstraints then reaching this line means none of them caused us to return true above, so return false.
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
		if(obj instanceof OrConstraint)
			return super.equals(obj); // CompositeConstraint#equals()
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // CompositeConstraint#hashCode()
		hash = 31 * hash + "OR".hashCode(); // to differentiate from AndConstraint
		return hash;
	}

}
