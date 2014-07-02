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

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		for(Constraint subConstraint : constraints)
			if(subConstraint._isValid(record))
				return true;
		return !hasSubConstraints(); // if we do not have subConstraints then any record is valid, if we *do* have subconstraints then reaching this line means none of them caused us to return true above, so return false.
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

}
