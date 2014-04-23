package uk.ac.ucl.excites.sapelli.storage.model;

import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;

/**
 * Class representing a foreign key, used to reference a record of another ("foreign") schema.
 * Implemented as a Record subclass, with an {@link Index} instance (the primary key of the foreign schema) as its schema.
 * 
 * @author mstevens
 */
public class ForeignKey extends Record
{
	
	static private final long serialVersionUID = 2L;
	
	private Schema foreignSchema;
	
	/**
	 * @param foreignSchema
	 * @throws NullPointerException	if the foreignSchema does not have a primary key set
	 */
	public ForeignKey(Schema foreignSchema)
	{
		 /* We use the foreign schema's primary key (an instance of Index, a subclass
		  * of Schema) as the schema for this record (ForeignKey is a subclass of Record) */
		super(foreignSchema.getPrimaryKey());
		this.foreignSchema = foreignSchema;
	}
	
	/**
	 * @param foreignRecord
	 * @throws NullPointerException	if the Schema of the foreignRecord does not have a primary key set
	 */
	public ForeignKey(Record foreignRecord)
	{
		this(foreignRecord.schema);
		
		// Copy the key part values:
		for(Column<?> keyPartCol : getIndex().getColumns(false))
		{
			Object keyPartValue = keyPartCol.retrieveValueCopy(foreignRecord);
			if(keyPartValue == null)
				throw new IllegalArgumentException("Cannot construct ForeignKey from record because key part \"" + keyPartCol.getName() + "\" has not been set");
			setValue(keyPartCol, keyPartValue);
		}
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param foreignKey
	 */
	public ForeignKey(ForeignKey foreignKey)
	{
		super(foreignKey);
		this.foreignSchema = foreignKey.foreignSchema;
	}
	
	/**
	 * @return the primary key of the foreignSchema 
	 */
	public Index getIndex()
	{
		return (Index) schema; // equivalent to: foreignSchema.getPrimaryKey()
	}
	
	/**
	 * @return the foreignSchema
	 */
	public Schema getForeignSchema()
	{
		return foreignSchema;
	}
	
	public SingleRecordQuery getForeignRecordQuery()
	{
		if(!isFilled())
			throw new IllegalStateException("All values of the key must be set before a query can be created!");
		// Match for key parts:
		AndConstraint andConstraint = new AndConstraint();
		for(Column<?> keyPartCol : getIndex().getColumns(false)) //TODO figure out why it didn't work with eqConstr & try to fix it, because the cast to comparatorCols is asking for trouble
			//andConstraint.addConstraint(new EqualityConstraint(keyPartCol, keyPartCol.retrieveValue(this)));
			andConstraint.addConstraint(new RuleConstraint((ComparatorColumn<?>) keyPartCol, RuleConstraint.Comparison.EQUAL, keyPartCol.retrieveValue(this)));
		// Single record query:
		return new FirstRecordQuery(new RecordsQuery(foreignSchema, andConstraint));
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + foreignSchema.hashCode();
		return hash;
	}
	
}
