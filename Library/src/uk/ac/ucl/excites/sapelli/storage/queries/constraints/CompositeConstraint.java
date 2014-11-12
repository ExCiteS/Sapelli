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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author mstevens
 *
 */
public abstract class CompositeConstraint extends Constraint
{

	private List<Constraint> constraints;

	/**
	 * @param constraints
	 */
	public CompositeConstraint(Constraint... constraints)
	{
		if(constraints != null)
			for(Constraint c : constraints)
				addConstraint(c);
	}
	
	public void addConstraint(Constraint constraint)
	{
		// Reduce if possible:
		constraint = Constraint.Reduce(constraint);
		// Null check:
		if(constraint == null)
			return;
		// Flatten instance of same CompositeConstraint subclass when associative:
		else if(this.getClass().isInstance(constraint) && isAssociative())
		{
			for(Constraint subConstraint : ((CompositeConstraint) constraint).constraints)
				addConstraint(subConstraint); // recursive call
		}
		// Add real subconstraint:
		else
		{
			if(constraints == null) // create collection if necessary
				constraints = new ArrayList<Constraint>();
			constraints.add(constraint);
		}
	}

	/**
	 * Reduce unnecessary (Composite)constraint instances
	 * 
	 * @return
	 */
	@Override
	public Constraint reduce()
	{
		if(!hasSubConstraints())
			return null;
		else if(constraints.size() == 1)
			return getSubConstraints().get(0).reduce();
		else
			return this;
	}
	
	public List<Constraint> getSubConstraints()
	{
		return constraints != null ? constraints : Collections.<Constraint> emptyList();
	}
	
	public boolean hasSubConstraints()
	{
		return constraints != null; // no need to check isEmpty(), the constraints collection is private and only created if there is at least 1 subConstraint
	}
	
	protected abstract boolean isAssociative();

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof CompositeConstraint)
		{
			CompositeConstraint that = (CompositeConstraint) obj;
			return this.getSubConstraints().equals(that.getSubConstraints());
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + getSubConstraints().hashCode();
		return hash;
	}
	
}
