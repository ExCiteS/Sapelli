/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.ColumnVisitor;
import uk.ac.ucl.excites.sapelli.storage.model.ForeignKey;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * A composite column holding columns that store the parts of a foreign key
 * 
 * @author mstevens
 */
public class ForeignKeyColumn extends RecordColumn<ForeignKey>
{

	private Schema foreignSchema;
	
	/**
	 * @param name
	 * @param foreignSchema
	 * @param optional
	 * @throws NullPointerException	if the foreignSchema does not have a primary key set
	 */
	public ForeignKeyColumn(String name, Schema foreignSchema, boolean optional)
	{
		super(ForeignKey.class, name, foreignSchema.getPrimaryKey() /* Index instance, a subclass of Schema */, optional);
		this.foreignSchema = foreignSchema;
	}

	@Override
	public ForeignKeyColumn copy()
	{
		return new ForeignKeyColumn(name, foreignSchema, optional);
	}

	@Override
	protected ForeignKey getNewRecord()
	{
		return new ForeignKey(foreignSchema);
	}

	@Override
	protected ForeignKey copy(ForeignKey value)
	{
		return new ForeignKey(value);
	}
	
	@Override
	public void accept(ColumnVisitor visitor)
	{
		if(visitor.isLocationSelfTraversalAllowed())
			super.accept(visitor, true);
		else
			visitor.visit(this);
	}

}
