package uk.ac.ucl.excites.storage.model;

import java.io.IOException;
import java.text.ParseException;

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

	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 * @throws ParseException
	 */
	@Override
	protected Float parse(String value) throws ParseException
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
	public boolean isVariableSize()
	{
		return false;
	}

	@Override
	public int getSize()
	{
		return Float.SIZE;
	}

	@Override
	protected String toString(Float value)
	{
		return value.toString();
	}

}
