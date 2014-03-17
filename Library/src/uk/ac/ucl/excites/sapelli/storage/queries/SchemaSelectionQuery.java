/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class SchemaSelectionQuery extends SelectionQuery
{

	private final Schema schema;
	
	public SchemaSelectionQuery(Schema schema)
	{
		if(schema == null)
			throw new NullPointerException("Schema cannot be null");
		this.schema = schema;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.SelectionQuery#isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public boolean isValid(Record record)
	{
		return this.schema.equals(record.getSchema()); // Note: only usageID & usageSubID are checked now, maybe we want to check more?
	}

	/**
	 * @return the schema
	 */
	public Schema getSchema()
	{
		return schema;
	}

	@Override
	protected void accept(QueryBuilder builder)
	{
		builder.visit(this);
	}

}
