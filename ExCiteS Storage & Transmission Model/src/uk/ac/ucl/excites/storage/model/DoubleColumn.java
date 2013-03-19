package uk.ac.ucl.excites.storage.model;

import java.io.IOException;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;

/**
 * A column for 64 bit floating point numbers (doubles)
 * 
 * @author mstevens
 */
public class DoubleColumn extends Column<Double>
{	
	
	public DoubleColumn(String name, boolean optional)
	{
		super(name, optional);
	}

	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 * @throws NumberFormatException
	 */
	@Override
	protected Double parse(String value) throws NumberFormatException
	{
		return Double.valueOf(value);
	}

	@Override
	protected void validate(Double value) throws IllegalArgumentException
	{
		//Does nothing because we allow all doubles (null check happens in super class)
	}

	@Override
	protected void write(Double value, BitOutputStream bitStream) throws IOException
	{
		bitStream.write(value);
	}

	@Override
	protected Double read(BitInputStream bitStream) throws IOException
	{
		return bitStream.readDouble();
	}

	@Override
	public boolean isVariableSize()
	{
		return false;
	}

	@Override
	public int getSize()
	{
		return Double.SIZE;
	}

	@Override
	protected String toString(Double value)
	{
		return value.toString();
	}

}
