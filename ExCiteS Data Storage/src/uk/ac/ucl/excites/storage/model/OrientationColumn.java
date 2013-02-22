/**
 * 
 */
package uk.ac.ucl.excites.storage.model;

import java.io.IOException;
import java.text.ParseException;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.types.Orientation;

/**
 * @author mstevens
 *
 */
public class OrientationColumn extends Column<Orientation>
{

	private boolean storeX;
	private boolean storeY;
	private boolean storeZ;
	
	/**
	 * 
	 */
	public OrientationColumn(String name, boolean optional, boolean storeX, boolean storeY, boolean storeZ)
	{
		super(name, optional);
		this.storeX = storeX;
		this.storeY = storeY;
		this.storeZ = storeZ;
	}

	@Override
	protected Orientation parse(String value) throws ParseException, IllegalArgumentException
	{
		return Orientation.Parse(value);
	}

	@Override
	protected String toString(Orientation value)
	{
		return value.toString();
	}

	@Override
	protected void write(Orientation value, BitOutputStream bitStream) throws IOException
	{
		if(storeX && value.hasX())
			bitStream.write(value.getX());
		if(storeY && value.hasY())
			bitStream.write(value.getY());
		if(storeZ && value.hasZ())
			bitStream.write(value.getZ());
	}

	@Override
	protected Orientation read(BitInputStream bitStream) throws IOException
	{
		Float x = null, y = null, z = null;
		if(storeX)
			x = bitStream.readFloat();
		if(storeY)
			y = bitStream.readFloat();
		if(storeZ)
			z = bitStream.readFloat();
		return new Orientation(x, y, z);
	}

	@Override
	protected void validate(Orientation value) throws IllegalArgumentException
	{
		//does nothing
	}

	@Override
	public boolean isVariableSize()
	{
		return false;
	}

	@Override
	public int getSize()
	{
		return (storeX ? Float.SIZE : 0) + (storeY ? Float.SIZE : 0) + (storeZ ? Float.SIZE : 0);
	}

}
