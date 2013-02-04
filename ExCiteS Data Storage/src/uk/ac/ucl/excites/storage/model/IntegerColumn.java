package uk.ac.ucl.excites.storage.model;
/**
 * 
 */


import java.io.IOException;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;



/**
 * A column for integers up to 64bits
 * 
 * @author mstevens
 *
 */
public class IntegerColumn extends Column<Long>
{
	
	//STATICS
	private static final boolean DEFAULT_SIGNEDNESS = true; //use signed integers by default
	private static final int DEFAULT_SIZE_BITS = 32; //use 32 bit integers by default
	
	//DYNAMICS
	private int size; //size in number of bits
	private boolean signed;
	private IntegerRangeMapping rangeMapping;
	
	public IntegerColumn(String name, int sizeBits)
	{
		this(name, DEFAULT_SIGNEDNESS, sizeBits);
	}
	
	public IntegerColumn(String name)
	{
		this(name, DEFAULT_SIGNEDNESS, DEFAULT_SIZE_BITS);
	}
	
	public IntegerColumn(String name, boolean signed)
	{
		this(name, signed, DEFAULT_SIZE_BITS);
	}
	
	/**
	 * @param name
	 * @param signed
	 * @param sizeBits size in number of bits (minimum 1, maximum 64)
	 */
	public IntegerColumn(String name, boolean signed, int sizeBits)
	{
		super(name);
		if(size < 1 || size > 64)
			throw new IllegalArgumentException("Invalid size (" + sizeBits + "). Size must be between 1 and 64 bits.");
		this.size = sizeBits;
		this.signed = signed;
	}

	/**
	 * Creates an IntegerColumn that is just big enough to be able to store any interger
	 * from the range [minLogicalValue; maxLogicalValue] (inclusive).
	 * 
	 * @param name
	 * @param minLogicalValue
	 * @param maxLogicalValue
	 */
	public IntegerColumn(String name, int minLogicalValue, int maxLogicalValue)
	{
		super(name);
		this.rangeMapping = new IntegerRangeMapping(minLogicalValue, maxLogicalValue);
		this.size = rangeMapping.getSize();
		this.signed = false;
	}
	
	@Override
	protected Long parse(String value)
	{
		return Long.parseLong(value);
	}

	@Override
	protected void validate(Long value) throws IllegalArgumentException
	{
//		if(rangeMapping != null)
//			value = rangeMapping.toRawValue(value);
		//Size checks:
		if(signed)
		{	//Signed
			if(value < (long) (- Math.pow(2, size - 1)) || value > (long) (Math.pow(2, size - 1) - 1))
				throw new IllegalArgumentException("Signed value (" + value + ") does not fit in " + size + " bits.");
		}
		else
		{	//Unsigned
			if(value < 0l)
				throw new IllegalArgumentException("Cannot write negative value as unsigned interger.");
			if(value > (long) (Math.pow(2, size) - 1))
				throw new IllegalArgumentException("Unsigned value (" + value + ") does not fit in " + size + " bits.");
		}
		//Signedness check
		//TODO how does shifting affect signedness and the test below?
		if(!signed && value < 0)
			throw new IllegalArgumentException("Cannot store negative value as unsigned integer");
	}

//	@Override
//	protected void write(Long value, BitOutputStream bitStream) throws IOException
//	{
//		bitStream.write(value - shift, size, signed);
//	}

//	@Override
//	protected Long read(BitInputStream bitStream) throws IOException
//	{
//		return 0 /*TODO*/ + shift;
//	}
	
	/**
	 * @return the size in number of bits
	 */
	public int getSize()
	{
		return size;
	}

	/**
	 * @return the signed
	 */
	public boolean isSigned()
	{
		return signed;
	}

	@Override
	public boolean isVariableSize()
	{
		return false;
	}

	@Override
	protected void write(Long value, BitOutputStream bitStream) throws IOException
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	protected Long read(BitInputStream bitStream) throws IOException
	{
		// TODO Auto-generated method stub
		return null;
	}
	
}
