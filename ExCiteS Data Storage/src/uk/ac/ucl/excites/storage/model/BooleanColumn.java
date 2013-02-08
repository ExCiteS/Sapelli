/**
 * 
 */
package uk.ac.ucl.excites.storage.model;

import java.io.IOException;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;

/**
 * @author mstevens
 *
 */
public class BooleanColumn extends Column<Boolean>
{

	/**
	 * @param name
	 */
	public BooleanColumn(String name, boolean optional)
	{
		super(name, optional);
	}

	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 */
	@Override
	protected Boolean parse(String value)
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
	public boolean isVariableSize()
	{
		return false;
	}

	@Override
	public int getSize()
	{
		return 1;
	}

}
