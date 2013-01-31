package uk.ac.ucl.excites.transmission.lib.model;

import java.io.IOException;

import uk.ac.ucl.excites.transmission.lib.io.BitInputStream;
import uk.ac.ucl.excites.transmission.lib.io.BitOutputStream;

/**
 * A column for 64 bit floating point numbers (doubles)
 * 
 * @author mstevens
 */
public class DoubleColumn extends Column<Double>
{	
	
	protected DoubleColumn(String name)
	{
		super(name);
	}

	@Override
	protected Double parse(String value)
	{
		return Double.parseDouble(value);
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

}
