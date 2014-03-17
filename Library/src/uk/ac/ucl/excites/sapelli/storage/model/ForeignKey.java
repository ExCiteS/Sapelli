package uk.ac.ucl.excites.sapelli.storage.model;

import uk.ac.ucl.excites.sapelli.storage.queries.CompositeSelectionQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.EqualitySelectionQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SchemaSelectionQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SelectionQuery;

/**
 * Class representing a foreign key, used to reference a record of another ("foreign") schema.
 * Implemented as a Record subclass, with an {@link Index} instance (the primary key of the foreign schema) as its schema.
 * 
 * @author mstevens
 */
public class ForeignKey extends Record
{
	
	private Schema foreignSchema;
	
	/**
	 * @param foreignSchema
	 * @throws NullPointerException	if the foreignSchema does not have a primary key set
	 */
	public ForeignKey(Schema foreignSchema)
	{
		super(foreignSchema.getPrimaryKey()); //the primary key of the foreign schema (which is an instance of Index, a subclass of Schema)
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
		for(Column<?> keyPartCol : getIndex().getColumns())
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
	
	public SelectionQuery getSelectionQuery()
	{
		if(!isFilled())
			throw new IllegalStateException("All values of the key must be set before a query can be created!");
		// Query for key parts:
		EqualitySelectionQuery eqQuery = new EqualitySelectionQuery();
		for(Column<?> keyPartCol : getIndex().getColumns())
			eqQuery.addColumnValue(keyPartCol, keyPartCol.retrieveValue(this));
		// Combine with schema checking query:
		return new CompositeSelectionQuery(new SchemaSelectionQuery(foreignSchema), eqQuery);
	}
	
}
