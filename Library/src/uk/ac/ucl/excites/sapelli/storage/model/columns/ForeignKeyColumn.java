/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ForeignKey;
import uk.ac.ucl.excites.sapelli.storage.model.Index;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * A composite column holding columns that store the parts of a foreign key
 * 
 * @author mstevens
 */
public class ForeignKeyColumn extends RecordColumn<ForeignKey>
{

	/**
	 * @param name
	 * @param foreignSchema
	 * @param optional
	 * @throws NullPointerException	if the foreignSchema does not have a primary key set
	 */
	public ForeignKeyColumn(String name, Schema foreignSchema, boolean optional)
	{
		this(name, foreignSchema.getPrimaryKey() /* Index instance, a subclass of Schema */, optional);
	}
	
	private ForeignKeyColumn(String name, Index foreignIndex, boolean optional)
	{
		super(ForeignKey.class, name, foreignIndex, optional);
	}

	@Override
	public Column<ForeignKey> copy()
	{
		return new ForeignKeyColumn(name, schema, optional);
	}

	@Override
	protected ForeignKey getNewRecord()
	{
		return new ForeignKey((Index) schema); // don't remove the cast!
	}
	
	@Override
	protected boolean equalRestrictions(Column<ForeignKey> otherColumn)
	{
		return schema.equals(((ForeignKeyColumn) otherColumn));
	}

	@Override
	protected ForeignKey copy(ForeignKey value)
	{
		return new ForeignKey(value);
	}

}
