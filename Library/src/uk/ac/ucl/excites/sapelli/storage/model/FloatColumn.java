package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;

/**
 * A column for 32 bit floating point numbers (floats)
 * 
 * @author mstevens
 */
public class FloatColumn extends Column<Float>
{	
	
	public FloatColumn(String name, boolean optional)
	{
		super(Float.class, name, optional);
	}

	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 * @throws NumberFormatException
	 */
	@Override
	protected Float parse(String value) throws NumberFormatException
	{
		return Float.valueOf(value);
	}

	@Override
	protected void validate(Float value) throws IllegalArgumentException
	{
		//Does nothing because we allow all floats (null check happens in super class)
	}

	@Override
	protected void write(Float value, BitOutputStream bitStream) throws IOException
	{
		bitStream.write(value);
	}

	@Override
	protected Float read(BitInputStream bitStream) throws IOException
	{
		return bitStream.readFloat();
	}
	
	@Override
	protected int _getMinimumSize()
	{
		return Float.SIZE;
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return Float.SIZE;
	}

	@Override
	protected String toString(Float value)
	{
		return value.toString();
	}

	@Override
	protected boolean equalRestrictions(Column<Float> otherColumn)
	{
		return (otherColumn instanceof FloatColumn);		
	}

	@Override
	protected Float copy(Float value)
	{
		return Float.valueOf(value);
	}
	
}
