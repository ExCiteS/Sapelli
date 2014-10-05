/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for integers with configurable size up to 64 bits
 * 
 * @author mstevens
 */
public class IntegerColumn extends ComparableColumn<Long>
{
	
	//STATICS
	private static final long serialVersionUID = 2L;
	
	private static final boolean DEFAULT_SIGNEDNESS = true; //allow signed values by default
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
		this.size = rangeMapping.size();
		this.signed = false;
	}
	
	@Override
	public IntegerColumn copy()
	{
		if(rangeMapping == null)
			return new IntegerColumn(name, optional, signed, size);
		else
			return new IntegerColumn(name, optional, rangeMapping);
	}
	
	/**
	 * Integer version of {@link IntegerColumn#storeValue(Record, Long)}
	 * 
	 * @param record
	 * @param value
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	public void storeValue(Record record, Integer value) throws IllegalArgumentException, NullPointerException
	{
		Long longValue = (value != null ? Long.valueOf(value.intValue()) : null);
		storeValue(record, longValue);
	}
	
	/**
	 * Stores the given Object value in this column on the given record.
	 * Overridden to deal with Integer & Short (and perhaps other Numbers) being passed a Objects.
	 *
	 * @param record
	 * @param value (as object, may be null if column is optional)
	 * @throws IllegalArgumentException in case of a schema mismatch or invalid value
	 * @throws NullPointerException if value is null on an non-optional column
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#storeObject(uk.ac.ucl.excites.sapelli.storage.model.Record, java.lang.Object)
	 */
	@Override
	public void storeObject(Record record, Object value) throws IllegalArgumentException, NullPointerException, ClassCastException
	{
		super.storeObject(record, ((Number) value).longValue());
	}
	
	/**
	 * @param record
	 * @param nullReplacement
	 * @return
	 */
	public long getPrimitiveLong(Record record, long nullReplacement)
	{
		Long longValue = retrieveValue(record);
		if(longValue == null)
			return nullReplacement;
		return longValue.longValue();
	}
	
	/**
	 * @param record
	 * @param nullReplacement
	 * @return
	 */
	public int getPrimitiveInt(Record record, int nullReplacement)
	{
		Long longValue = retrieveValue(record);
		if(longValue == null)
			return nullReplacement;
		return longValue.intValue();
	}
	
	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 * @throws NumberFormatException
	 */
	@Override
	public Long parse(String value) throws NumberFormatException
	{
		return Long.valueOf(value);
	}

	@Override
	protected void validate(Long value) throws IllegalArgumentException
	{
		if(rangeMapping != null && !rangeMapping.inRange(value))
			throw new IllegalArgumentException("The value (" + value + ") is not in the allowed range of [" + rangeMapping.lowBound() + ", " + rangeMapping.highBound() + "].");
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
			return rangeMapping.lowBound();
		else
			return (signed ?	(long) (- Math.pow(2, size - 1)) : 
								0l);
	}

	public long getMaxValue()
	{
		if(rangeMapping != null)
			return rangeMapping.highBound();
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
	public String toString(Long value)
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
	public String getTypeString()
	{
		return Integer.class.getSimpleName();
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

	@Override
	protected int compareNonNullValues(Long lhs, Long rhs)
	{
		return lhs.compareTo(rhs);
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + size;
		hash = 31 * hash + (signed ? 0 : 1);
		hash = 31 * hash + (rangeMapping == null ? 0 : rangeMapping.hashCode());
		return hash;
	}
	
}
