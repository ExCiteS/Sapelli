/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.examples;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.queries.CompositeSelectionQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.EqualitySelectionQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.QueryBuilder;
import uk.ac.ucl.excites.sapelli.storage.queries.SchemaSelectionQuery;
import uk.ac.ucl.excites.sapelli.util.StringUtils;

/**
 * Example of how to generate SQL WHERE clauses from SelectionQueries
 * 
 * @author mstevens
 *
 */
public class SQLWhereClauseBuilder implements QueryBuilder
{
	
	private List<String> parts;

	public SQLWhereClauseBuilder()
	{
		this.parts = new ArrayList<String>();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.QueryBuilder#visit(uk.ac.ucl.excites.sapelli.storage.queries.SchemaSelectionQuery)
	 */
	@Override
	public void visit(SchemaSelectionQuery schemaQuery)
	{
		parts.add("usageID = " + schemaQuery.getSchema().getUsageID() + " AND usageSubID = " + schemaQuery.getSchema().getUsageSubID());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.QueryBuilder#visit(uk.ac.ucl.excites.sapelli.storage.queries.EqualitySelectionQuery)
	 */
	@Override
	public void visit(EqualitySelectionQuery equalityQuery)
	{
		for(Column<?> c : equalityQuery.getColumns())
			parts.add(c.getName() + " = " + c.objectToString(equalityQuery.getValueFor(c))); //TODO use a ColumnVisitor to obtain proper SQL-compatible String version of value
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.QueryBuilder#visit(uk.ac.ucl.excites.sapelli.storage.queries.CompositeSelectionQuery)
	 */
	@Override
	public void visit(CompositeSelectionQuery composityQuery)
	{
		// does nothing
	}
	
	public String toString()
	{
		if(!parts.isEmpty())
			return StringUtils.join(parts, " AND ");
		else
			return "";
	}

	@Override
	public boolean isCompsitesSelfTraversalAllowed()
	{
		return true;
	}

}
