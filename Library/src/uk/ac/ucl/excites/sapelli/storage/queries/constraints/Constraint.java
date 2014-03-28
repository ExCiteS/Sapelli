/**
 * 
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
		return record != null & _isValid(record);
	}
	
	/**
	 * @param record a guaranteed non-null {@link Record} instance
	 * @return
	 */
	protected abstract boolean _isValid(Record record);
		
	public abstract void accept(ConstraintVisitor visitor);
	
	public Constraint negate()
	{
		if(this instanceof NotConstraint)
			// Avoid double-negations:
			return ((NotConstraint) this).getNegatedConstraint();
		else
			return new NotConstraint(this);
	}

}
