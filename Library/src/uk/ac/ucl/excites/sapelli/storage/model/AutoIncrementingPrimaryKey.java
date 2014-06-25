/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;

/**
 * As special kind of {@link PrimaryKey} which comprises of a single IntegerColumn whose values are generated automatically (i.e. auto-incrementing) by the back-end database.
 * 
 * @author mstevens
 */
public class AutoIncrementingPrimaryKey extends PrimaryKey
{

	private static final long serialVersionUID = 2L;
	
	/**
	 * @param name
	 * @param intColumn
	 */
	public AutoIncrementingPrimaryKey(String name, IntegerColumn intColumn)
	{
		super(name, intColumn);
	}
	
	public IntegerColumn getColumn()
	{
		return (IntegerColumn) getColumn(0);
	}

	@Override
    public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof AutoIncrementingPrimaryKey)
			return super.equals(obj);
		return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + "AutoIncrement".hashCode(); // to differentiate from a normal PrimaryKey
		return hash;
	}
	
}
