/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

/**
 * @author mstevens
 *
 * @param <ST> source type
 * @param <VT> virtual type
 */
public abstract class ColumnVirtualiser<ST, VT>
{

	/**
	 * the "real" column
	 */
	private Column<ST> sourceColumn;
	
	/**
	 * @param sourceColumn the "real" column
	 */
	public ColumnVirtualiser(Column<ST> sourceColumn)
	{
		if(sourceColumn == null)
			throw new NullPointerException("sourceColumn cannot be null!");
		this.sourceColumn = sourceColumn;
	}
	
	public VT retrieveValue(Record record)
	{
		return mapValue(sourceColumn.retrieveValue(record));
	}
	
	public VT mapValue(ST value)
	{
		if(value == null)
			return null;
		return mapValueNonNull(value);
	}
	
	/**
	 * @param value assumed to be non-null
	 * @return
	 */
	protected abstract VT mapValueNonNull(ST value);

	/**
	 * @return the sourceColumn (the "real" one)
	 */
	public Column<ST> getSourceColumn()
	{
		return sourceColumn;
	}
	
	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + sourceColumn.hashCode();
		return hash;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj == null)
			return false;
		if(this == obj) // compare pointers first
			return true;
		if(this.getClass().isInstance(obj))
			return this.sourceColumn.equals(((ColumnVirtualiser<?, ?>) obj).sourceColumn);
		return false;
	}
	
}
