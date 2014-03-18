/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class RuleSelectionQuery extends SelectionQuery
{
	
	private final List<Rule> rules;
	
	public RuleSelectionQuery()
	{
		this.rules = new ArrayList<Rule>();
	}
	
	/**
	 * Note: we currently do not check whether the combination of added rules
	 * is consistent (e.g. it is currently possible to add "A > 5" and "A <= 3")
	 * 
	 * @param rule
	 */
	public void addRule(Rule rule)
	{
		rules.add(rule);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.SelectionQuery#isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean isValid(Record record)
	{
		for(Rule rule : rules)
		{
			if(!rule.isValid(record))
				return false;
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.SelectionQuery#accept(uk.ac.ucl.excites.sapelli.storage.queries.QueryBuilder)
	 */
	@Override
	protected void accept(QueryBuilder builder)
	{
		builder.visit(this);
	}
	
	/**
	 * @return the rules
	 */
	public List<Rule> getRules()
	{
		return rules;
	}

}
