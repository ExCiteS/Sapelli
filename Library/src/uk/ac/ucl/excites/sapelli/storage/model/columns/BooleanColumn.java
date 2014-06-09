/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparatorColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * @author mstevens
 *
 */
public class BooleanColumn extends ComparatorColumn<Boolean>
{

	static private final long serialVersionUID = 2L;
	
	/**
	 * @param name
	 */
	public BooleanColumn(String name, boolean optional)
	{
		super(Boolean.class, name, optional);
	}
	
	@Override
	public BooleanColumn copy()
	{
		return new BooleanColumn(name, optional);
	}
	
	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 */
	@Override
	public Boolean parse(String value)
	{
		return Boolean.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.storage.model.Column#write(java.lang.Object, uk.ac.ucl.excites.storage.io.BitOutputStream)
	 */
	@Override
	protected void write(Boolean value, BitOutputStream bitStream) throws IOException
	{
		bitStream.write(value);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.storage.model.Column#read(uk.ac.ucl.excites.storage.io.BitOutputStream)
	 */
	@Override
	protected Boolean read(BitInputStream bitStream) throws IOException
	{
		return bitStream.readBit();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(Boolean value) throws IllegalArgumentException
	{
		//Does nothing because we allow all booleans (null check happens in super class)
	}

	@Override
	protected int _getMinimumSize()
	{
		return 1;
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return 1;
	}

	@Override
	public String toString(Boolean value)
	{
		return value.toString();
	}

	@Override
	protected boolean equalRestrictions(Column<Boolean> otherColumn)
	{
		return (otherColumn instanceof BooleanColumn);		
	}

	@Override
	protected Boolean copy(Boolean value)
	{
		return Boolean.valueOf(value);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

	@Override
	protected int compareNonNullValues(Boolean lhs, Boolean rhs)
	{
		return lhs.compareTo(rhs);
	}

}
