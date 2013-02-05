package uk.ac.ucl.excites.storage.model;

import java.io.IOException;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;

/**
 * A column for 32 bit floating point numbers (floats)
 * 
 * @author mstevens
 */
public class FloatColumn extends Column<Float>
{	
	
	public FloatColumn(String name, boolean optional)
	{
		super(name, optional);
	}

	@Override
	protected Float parse(String value)
	{
		return Float.parseFloat(value);
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
	public boolean isVariableSize()
	{
		return false;
	}

	@Override
	public int getSize()
	{
		return Float.SIZE;
	}

}
