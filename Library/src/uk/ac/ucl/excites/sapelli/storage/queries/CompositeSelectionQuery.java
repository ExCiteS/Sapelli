/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class CompositeSelectionQuery extends SelectionQuery
{

	private final SelectionQuery[] subQueries;
	
	public CompositeSelectionQuery(SelectionQuery... subQueries)
	{
		this.subQueries = subQueries;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.SelectionQuery#isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean isValid(Record record)
	{
		for(SelectionQuery q : subQueries)
			if(!q.isValid(record))
				return false;
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.SelectionQuery#accept(uk.ac.ucl.excites.sapelli.storage.queries.QueryBuilder)
	 */
	@Override
	protected void accept(QueryBuilder builder)
	{
		if(builder.isCompsitesSelfTraversalAllowed())
		{	// the composite is allowed to traverse its subqueries
			for(SelectionQuery subQuery : subQueries)
				subQuery.accept(builder);
		}
		else
			builder.visit(this); // query builder will traverse subqueries
	}
	
	public SelectionQuery[] getSubQueries()
	{
		return subQueries;
	}

}
