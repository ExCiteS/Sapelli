/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.ForeignKey;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A composite column holding columns that store the parts of a foreign key
 * 
 * @author mstevens
 */
public class ForeignKeyColumn extends RecordColumn<ForeignKey>
{
	
	static private final long serialVersionUID = 2L;

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
	public ForeignKey getNewRecord()
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
		if(visitor.allowForeignKeySelfTraversal())
			super.accept(visitor, true);
		else
			visitor.visit(this);
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + foreignSchema.hashCode();
		return hash;
	}

}
