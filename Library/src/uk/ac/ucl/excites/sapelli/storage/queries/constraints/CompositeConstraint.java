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
import java.util.List;

/**
 * @author mstevens
 *
 */
public abstract class CompositeConstraint extends Constraint
{

	protected final List<Constraint> constraints;

	/**
	 * @param constraints
	 */
	public CompositeConstraint(Constraint... constraints)
	{
		this.constraints = new ArrayList<Constraint>();
		if(constraints != null)
			for(Constraint c : constraints)
				addConstraint(c);
	}
	
	public void addConstraint(Constraint constraint)
	{
		if(constraint == null)
			return;
		if(this.getClass().isInstance(constraint) && isAssociative())
			// Flatten instance of same CompositeConstraint subclass when associative:
			for(Constraint subConstraint : ((CompositeConstraint) constraint).constraints)
				this.constraints.add(subConstraint);
		else
			this.constraints.add(constraint);
	}

	public List<Constraint> getSubConstraints()
	{
		return constraints;
	}

	public boolean hasSubConstraints()
	{
		return !constraints.isEmpty();
	}
	
	protected abstract boolean isAssociative();

}
