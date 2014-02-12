package uk.ac.ucl.excites.sapelli.storage.model;
/**
 * 
 */


import java.io.IOException;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;

/**
 * A column for integers up to 64 bits
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
	
	/**
	 * Creates an IntegerColumn with the default number of bits ({@value #DEFAULT_SIZE_BITS}) and the default signedness ({@value #DEFAULT_SIGNEDNESS})
	 * 
	 * @param name
	 * @param optional
	 */
	public IntegerColumn(String name, boolean optional)
	{
		this(name, optional, DEFAULT_SIGNEDNESS, DEFAULT_SIZE_BITS);
	}

	/**
	 * Creates an IntegerColumn of the specified size (in bits) and the default signedness ({@value #DEFAULT_SIGNEDNESS})
	 * 
	 * @param name
	 * @param optional
	 * @param sizeBits	size in number of bits (minimum 1, maximum 64)
	 */
	public IntegerColumn(String name, boolean optional, int sizeBits)
	{
		this(name, optional, DEFAULT_SIGNEDNESS, sizeBits);
	}
	
	/**
	 * Creates an IntegerColumn with the default number of bits ({@value #DEFAULT_SIZE_BITS}) and the specified signedness
	 * 
	 * @param name
	 * @param optional
	 * @param signed
	 */
	public IntegerColumn(String name, boolean optional, boolean signed)
	{
		this(name, optional, signed, DEFAULT_SIZE_BITS);
	}
	
	/**
	 * Creates an IntegerColumn of the specified size (in bits) and the specified signedness
	 * 
	 * @param name
	 * @param optional
	 * @param signed
	 * @param sizeBits	size in number of bits (minimum 1, maximum 64)
	 */
	public IntegerColumn(String name, boolean optional, boolean signed, int sizeBits)
	{
		super(Long.class, name, optional);
		if(sizeBits < 1 || sizeBits > 64)
			throw new IllegalArgumentException("Invalid size (" + sizeBits + "). Size must be between 1 and 64 bits.");
		this.size = sizeBits;
		this.signed = signed;
	}

	/**
	 * Creates an IntegerColumn that is just big enough to be able to store any integer
	 * from the range [minLogicalValue; maxLogicalValue] (inclusive).
	 * 
	 * @param name
	 * @param optional
	 * @param minLogicalValue
	 * @param maxLogicalValue
	 */
	public IntegerColumn(String name, boolean optional, long minLogicalValue, long maxLogicalValue)
	{
		this(name, optional, new IntegerRangeMapping(minLogicalValue, maxLogicalValue));
	}
	
	/**
	 * Creates an IntegerColumn that is just big enough to be able to store values accepted by the provided IntegerRangeMapping
	 * 
	 * @param name
	 * @param optional
	 * @param rangeMapping
	 */
	public IntegerColumn(String name, boolean optional, IntegerRangeMapping rangeMapping)
	{
		super(Long.class, name, optional);
		this.rangeMapping = rangeMapping;
		this.size = rangeMapping.getSize();
		this.signed = false;
	}
	
	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 * @throws NumberFormatException
	 */
	@Override
	protected Long parse(String value) throws NumberFormatException
	{
		return Long.valueOf(value);
	}

	@Override
	protected void validate(Long value) throws IllegalArgumentException
	{
		if(rangeMapping != null && !rangeMapping.inRange(value))
			throw new IllegalArgumentException("The value (" + value + ") is not in the allowed range of [" + rangeMapping.getLowBound() + ", " + rangeMapping.getHighBound() + "].");
		else
		{
			//Size checks:
			if(signed)
			{	//Signed
				if(value < (long) (- Math.pow(2, size - 1)) || value > (long) (Math.pow(2, size - 1) - 1))
					throw new IllegalArgumentException("Signed value (" + value + ") does not fit in " + size + " bits.");
			}
			else
			{	//Unsigned
				if(value < 0l)
					throw new IllegalArgumentException("Cannot store negative value as unsigned interger.");
				if(value > (long) (Math.pow(2, size) - 1))
					throw new IllegalArgumentException("Unsigned value (" + value + ") does not fit in " + size + " bits.");
			}
		}
	}
	
	public long getMinValue()
	{
		if(rangeMapping != null)
			return rangeMapping.getLowBound();
		else
			return (signed ?	(long) (- Math.pow(2, size - 1)) : 
								0l);
	}

	public long getMaxValue()
	{
		if(rangeMapping != null)
			return rangeMapping.getHighBound();
		else
			return (signed ?	(long) (Math.pow(2, size - 1) - 1) : 
								(long) (Math.pow(2, size) - 1));
	}
	
	@Override
	protected void write(Long value, BitOutputStream bitStream) throws IOException
	{
		if(rangeMapping != null)
			rangeMapping.write(value, bitStream);
		else
			bitStream.write(value, size, signed);
	}

	@Override
	protected Long read(BitInputStream bitStream) throws IOException
	{
		if(rangeMapping != null)
			return rangeMapping.read(bitStream);
		else
			return bitStream.readInteger(size, signed);
	}
	
	@Override
	protected int _getMinimumSize()
	{
		return size;
	}
	
	@Override
	protected int _getMaximumSize()
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
	protected String toString(Long value)
	{
		return value.toString();
	}

	@Override
	protected boolean equalRestrictions(Column<Long> otherColumn)
	{
		if(otherColumn instanceof IntegerColumn)
		{
			IntegerColumn other = (IntegerColumn) otherColumn;
			return	this.size == other.size &&
					this.signed == other.signed &&
					(rangeMapping == null ? other.rangeMapping == null : this.rangeMapping.equals(other.rangeMapping));
		}
		else
			return false;
	}

	@Override
	protected Long copy(Long value)
	{
		return Long.valueOf(value);
	}
	
	/**
	 * Even though the type is actually Long we have called this column an "IntegerColumn" because the size can vary (so values are not necessarily 64bit longs) 
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#getTypeString()
	 */
	@Override
	protected String getTypeString()
	{
		return Integer.class.getSimpleName();
	}
	
}
