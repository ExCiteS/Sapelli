package uk.ac.ucl.excites.sapelli.storage.model;

/**
 * @author mstevens
 *
 */
public class ForeignKey extends Record
{
	
	/**
	 * @param foreignSchema
	 * @throws NullPointerException	if the foreignSchema does not have a primary key set
	 */
	public ForeignKey(Schema foreignSchema)
	{
		this(foreignSchema.getPrimaryKey()); //the primary key of the foreign schema (which is an instance of Index, a subclass of Schema) 	
	}
	
	/**
	 * @param foreignIndex
	 */
	public ForeignKey(Index foreignIndex)
	{
		super(foreignIndex);
	}
	
	/**
	 * @param foreignRecord
	 * @throws NullPointerException	if the Schema of the foreignRecord does not have a primary key set
	 */
	public ForeignKey(Record foreignRecord)
	{
		this(foreignRecord.schema);
		
		// Copy the key part values:
		for(Column<?> c : schema.getColumns())
			setValue(c, c.retrieveValue(foreignRecord));
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param foreignKey
	 */
	public ForeignKey(ForeignKey foreignKey)
	{
		super(foreignKey);
	}
	
}
