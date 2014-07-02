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

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;

/**
 * A class representing constraints on a {@link RecordsQuery}.
 * Similar to the conditions in a SQL WHERE clause.
 * 
 * @author mstevens
 */
public abstract class Constraint
{
	
	/**
	 * Filters a list of records based on certain criteria
	 * 
	 * @param records
	 * @return
	 */
	public List<Record> filter(List<Record> records)
	{
		List<Record> result = new ArrayList<Record>();
		for(Record r : records)
			if(isValid(r))
				result.add(r);
		return result;
	}

	public boolean isValid(Record record)
	{
		return record != null && _isValid(record);
	}
	
	/**
	 * @param record a guaranteed non-null {@link Record} instance
	 * @return
	 */
	protected abstract boolean _isValid(Record record);
		
	public abstract void accept(ConstraintVisitor visitor);
	
	public Constraint negate()
	{
		return NotConstraint.Negate(this); // will avoid double negations
	}

}
