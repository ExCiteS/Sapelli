/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

/**
 * Class representing a database index spanning one or more columns.
 * Implemented as a subclass of {@link Schema}.
 * 
 * @author mstevens
 */
public class Index extends Schema
{

	static private final long serialVersionUID = 2L;
	
	private boolean unique;
	
	public Index(String name, boolean unique, Column<?>... columns)
	{
		super(ReservedIDs.INDEX_SCHEMA.ordinal(), name);
		this.unique = unique;
		
		// Add columns:
		for(Column<?> iCol : columns)
			addColumn(iCol); // Note: the columns are not copied, just shared! (columns don't "know" their Schema(s) anyway)
		seal();
	}

	/**
	 * @return the unique
	 */
	public boolean isUnique()
	{
		return unique;
	}
	
	@Override
	public String toString()
	{
		return "Index " + name;
	}
	
	@Override
	public void addIndex(Index index, boolean useAsPrimaryKey)
	{
		throw new UnsupportedOperationException("Cannot add indexes to an index");
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + (unique ? 0 : 1);
		return hash;
	}
	
}
