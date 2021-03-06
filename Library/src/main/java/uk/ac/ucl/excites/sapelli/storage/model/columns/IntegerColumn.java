/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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
import java.math.BigInteger;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.BigIntegerUtils;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.util.InvalidColumnException;
import uk.ac.ucl.excites.sapelli.storage.util.InvalidValueException;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for integers with configurable size up to 64 bits
 * 
 * @author mstevens
 */
public class IntegerColumn extends NumberColumn<Long>
{
	
	// STATICS -----------------------------------------------------------
	private static final long serialVersionUID = 2L;
	
	private static final boolean DEFAULT_SIGNEDNESS = true; // allow signed values by default
	private static final int DEFAULT_SIZE_BITS = Integer.SIZE; // use 32 bit integers by default
	
	// DYNAMICS ----------------------------------------------------------
	private final int size; // size in number of bits
	private final boolean signed;
	private final IntegerRangeMapping rangeMapping;
	
	/**
	 * Creates an IntegerColumn with the default number of bits ({@value #DEFAULT_SIZE_BITS}) and the default signedness ({@value #DEFAULT_SIGNEDNESS})
	 * 
	 * @param name
	 * @param optional
	 */
	public IntegerColumn(String name, boolean optional)
	{
		this(name, optional, (Long) null);
	}
	
	/**
	 * Creates an IntegerColumn with the default number of bits ({@value #DEFAULT_SIZE_BITS}) and the default signedness ({@value #DEFAULT_SIGNEDNESS})
	 * 
	 * @param name
	 * @param optional
	 * @param defaultValue
	 */
	public IntegerColumn(String name, boolean optional, Long defaultValue)
	{
		this(name, optional, DEFAULT_SIGNEDNESS, DEFAULT_SIZE_BITS, defaultValue);
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
		this(name, optional, sizeBits, null);
	}
	
	/**
	 * Creates an IntegerColumn of the specified size (in bits) and the default signedness ({@value #DEFAULT_SIGNEDNESS})
	 * 
	 * @param name
	 * @param optional
	 * @param sizeBits	size in number of bits (minimum 1, maximum 64)
	 * @param defaultValue
	 */
	public IntegerColumn(String name, boolean optional, int sizeBits, Long defaultValue)
	{
		this(name, optional, DEFAULT_SIGNEDNESS, sizeBits, defaultValue);
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
		this(name, optional, signed, null);
	}
	
	/**
	 * Creates an IntegerColumn with the default number of bits ({@value #DEFAULT_SIZE_BITS}) and the specified signedness
	 * 
	 * @param name
	 * @param optional
	 * @param signed
	 * @param defaultValue
	 */
	public IntegerColumn(String name, boolean optional, boolean signed, Long defaultValue)
	{
		this(name, optional, signed, DEFAULT_SIZE_BITS, defaultValue);
	}
	
	/**
	 * Creates an IntegerColumn of the specified size (in bits) and the specified signedness
	 * 
	 * @param name
	 * @param optional
	 * @param signed
	 * @param sizeBits	size in number of bits (minimum 1, maximum 64 or 63, depending on signed)
	 */
	public IntegerColumn(String name, boolean optional, boolean signed, int sizeBits)
	{
		this(name, optional, signed, sizeBits, null);
	}
	
	/**
	 * Creates an IntegerColumn of the specified size (in bits) and the specified signedness
	 * 
	 * @param name
	 * @param optional
	 * @param signed
	 * @param sizeBits	size in number of bits (minimum 1, maximum 64 or 63, depending on signed)
	 * @param defaultValue
	 */
	public IntegerColumn(String name, boolean optional, boolean signed, int sizeBits, Long defaultValue)
	{
		this(name, optional, signed, sizeBits, false, defaultValue);
	}
	
	/**
	 * Creates an IntegerColumn of the specified size (in bits) and the specified signedness.
	 * {@code sizeBits} can be 0 but to avoid this being done by accident {@code allowEmpty} must be {@code true} in that case.
	 * 
	 * @param name
	 * @param optional
	 * @param signed
	 * @param sizeBits	size in number of bits (minimum 0 or 1, depending on allowEmpty; maximum 64 or 63, depending on signed)
	 * @param allowEmpty	whether or not to allow sizeBits to be 0
	 */
	public IntegerColumn(String name, boolean optional, boolean signed, int sizeBits, boolean allowEmpty)
	{
		this(name, optional, signed, sizeBits, allowEmpty, null);
	}
	
	/**
	 * Creates an IntegerColumn of the specified size (in bits) and the specified signedness.
	 * {@code sizeBits} can be 0 but to avoid this being done by accident {@code allowEmpty} must be {@code true} in that case.
	 * 
	 * @param name
	 * @param optional
	 * @param signed
	 * @param sizeBits	size in number of bits (minimum 0 or 1, depending on allowEmpty; maximum 64 or 63, depending on signed)
	 * @param allowEmpty	whether or not to allow sizeBits to be 0
	 * @param defaultValue
	 */
	public IntegerColumn(String name, boolean optional, boolean signed, int sizeBits, boolean allowEmpty, Long defaultValue)
	{
		super(name, optional, defaultValue);
		if(sizeBits < (allowEmpty ? 0 : 1) || sizeBits > (Long.SIZE - (signed ? 0 : 1)))
			throw new IllegalArgumentException(	"Invalid size (" + sizeBits + " bits), allowed range for " +
												(signed ? "" : "un") + "signed and " + (allowEmpty ? "" : "not ") + "allowed empty is [" +
												(allowEmpty ? 0 : 1) + ", " + (Long.SIZE - (signed ? 0 : 1)) + "] bits.");
		this.size = sizeBits;
		this.signed = signed;
		this.rangeMapping = null;
	}

	/**
	 * Creates an IntegerColumn that is just big enough to be able to store any integer
	 * from the range [minLogicalValue; maxLogicalValue] (inclusive).
	 * 
	 * @param name
	 * @param optional
	 * @param minLogicalValue
	 * @param maxLogicalValue	must be strictly larger than minLogicalValue
	 */
	public IntegerColumn(String name, boolean optional, long minLogicalValue, long maxLogicalValue)
	{
		this(name, optional, minLogicalValue, maxLogicalValue, null);
	}
	
	/**
	 * Creates an IntegerColumn that is just big enough to be able to store any integer
	 * from the range [minLogicalValue; maxLogicalValue] (inclusive).
	 * 
	 * @param name
	 * @param optional
	 * @param minLogicalValue
	 * @param maxLogicalValue	must be strictly larger than minLogicalValue
	 * @param defaultValue
	 */
	public IntegerColumn(String name, boolean optional, long minLogicalValue, long maxLogicalValue, Long defaultValue)
	{
		this(name, optional, minLogicalValue, maxLogicalValue, false, defaultValue);
	}
	
	/**
	 * Creates an IntegerColumn that is just big enough to be able to store any integer
	 * from the range [minLogicalValue; maxLogicalValue] (inclusive).
	 * 
	 * @param name
	 * @param optional
	 * @param minLogicalValue
	 * @param maxLogicalValue
	 * @param allowEmpty	when {@code false} minLogicalValue must be < maxLogicalValue, when {@code true} minLogicalValue must be <= maxLogicalValue
	 */
	public IntegerColumn(String name, boolean optional, long minLogicalValue, long maxLogicalValue, boolean allowEmpty)
	{
		this(name, optional, minLogicalValue, maxLogicalValue, allowEmpty, null);
	}
	
	/**
	 * Creates an IntegerColumn that is just big enough to be able to store any integer
	 * from the range [minLogicalValue; maxLogicalValue] (inclusive).
	 * 
	 * @param name
	 * @param optional
	 * @param minLogicalValue
	 * @param maxLogicalValue
	 * @param allowEmpty	when {@code false} minLogicalValue must be < maxLogicalValue, when {@code true} minLogicalValue must be <= maxLogicalValue
	 * @param defaultValue
	 */
	public IntegerColumn(String name, boolean optional, long minLogicalValue, long maxLogicalValue, boolean allowEmpty, Long defaultValue)
	{
		this(name, optional, new IntegerRangeMapping(minLogicalValue, maxLogicalValue, allowEmpty), defaultValue);
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
		this(name, optional, rangeMapping, null);
	}
	
	/**
	 * Creates an IntegerColumn that is just big enough to be able to store values accepted by the provided IntegerRangeMapping
	 * 
	 * @param name
	 * @param optional
	 * @param rangeMapping
	 * @param defaultValue
	 */
	public IntegerColumn(String name, boolean optional, IntegerRangeMapping rangeMapping, Long defaultValue)
	{
		super(name, optional, defaultValue);
		this.rangeMapping = rangeMapping;
		if(	rangeMapping.lowBound().compareTo(BigInteger.valueOf(Long.MIN_VALUE)) < 0 ||
			rangeMapping.highBound(false).compareTo(BigInteger.valueOf(Long.MAX_VALUE)) > 0)
			throw new IllegalArgumentException("The given rangeMapping accepts BigInteger values of magnitudes beyond the bounds of Long integers");
		this.size = rangeMapping.size();
		this.signed = rangeMapping.lowBound().compareTo(BigInteger.ZERO) < 0;
	}
	
	@Override
	protected IntegerColumn createCopy()
	{
		if(rangeMapping == null)
			return new IntegerColumn(name, optional, signed, size, defaultValue);
		else
			return new IntegerColumn(name, optional, new IntegerRangeMapping(rangeMapping), defaultValue);
	}
	
	/**
	 * Integer version of {@link IntegerColumn#storeValue(Record, Long)}
	 * 
	 * @param valueSet
	 * @param value
	 * @throws InvalidColumnException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws InvalidValueException when the given value is invalid
	 * @throws NullPointerException if value is {@code null} on an non-optional column, or if the valueSet is {@code null}
	 */
	public void storeValue(ValueSet<?> valueSet, Integer value) throws InvalidColumnException, InvalidValueException, NullPointerException
	{
		storeValue(valueSet, convert(value));
	}

	/**
	 * Converts {@link Number}s ({@link Integer}s, {@link Short}s, etc.) and {@link Enum}s to {@link Long}s.
	 * 
	 * @param value possibly null
	 * @return
	 * @throws ClassCastException when the value is not a {@link Number}
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#convert(java.lang.Object)
	 */
	@Override
	public Long convert(Object value)
	{
		// Null:
		if(value == null)
			return null;
		// Already a Long:
		if(value instanceof Long)
			return (Long) value;
		// Enum:
		if(value instanceof Enum)
			return Long.valueOf(((Enum<?>) value).ordinal());
		// Number:
		return Long.valueOf(((Number) value).longValue()); // will fail if object is not a Number instance	
	}

	/**
	 * @param valueSet
	 * @param nullReplacement
	 * @return
	 * @throws NullPointerException if the given {@link ValueSet} is {@code null}
	 * @throws InvalidColumnException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 */
	public long getPrimitiveLong(ValueSet<?> valueSet, long nullReplacement) throws NullPointerException, InvalidColumnException
	{
		Long longValue = retrieveValue(valueSet);
		if(longValue == null)
			return nullReplacement;
		return longValue.longValue();
	}
	
	/**
	 * @param valueSet
	 * @param nullReplacement
	 * @return
	 * @throws NullPointerException if the given {@link ValueSet} is {@code null}
	 * @throws InvalidColumnException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 */
	public int getPrimitiveInt(ValueSet<?> valueSet, int nullReplacement) throws NullPointerException, InvalidColumnException
	{
		Long longValue = retrieveValue(valueSet);
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
	protected void validate(Long value) throws InvalidValueException
	{
		if(rangeMapping != null && !rangeMapping.inStrictRange(value))
			throw new InvalidValueException("The value (" + value + ") is not in the allowed range: " + rangeMapping.getStrictRangeString() + ".", this);
		if(value < getMinValue() || value > getMaxValue())
		{
			if(!signed && value < 0l)
				throw new InvalidValueException("Cannot store negative value (" + value + ") as an unsigned integer.", this);
			//else:
			throw new InvalidValueException("The value (" + value + ") does not fit a" + (signed ? " " : "n un") + "signed integer of " + size + " bits (allowed range: " + IntegerRangeMapping.GetRangeString(getMinValue(), getMaxValue()) + ").", this);
		}
	}
	
	public long getMinValue()
	{
		return (rangeMapping != null ?	rangeMapping.lowBound() :
										BigIntegerUtils.GetMinValue(size, signed)).longValue();
	}

	public long getMaxValue()
	{
		return (rangeMapping != null ?	rangeMapping.highBound() :
										BigIntegerUtils.GetMaxValue(size, signed)).longValue();
	}
	
	@Override
	protected void write(Long value, BitOutputStream bitStream, boolean lossless) throws IOException
	{
		if(rangeMapping != null)
			rangeMapping.write(value, bitStream);
		else
			bitStream.write(value, size, signed);
	}

	@Override
	protected Long read(BitInputStream bitStream, boolean lossless) throws IOException
	{
		if(rangeMapping != null)
			return rangeMapping.read(bitStream).longValue();
		else
			return bitStream.readInteger(size, signed);
	}
	
	@Override
	protected int getMinimumValueSize(boolean lossless)
	{
		return size;
	}
	
	@Override
	protected int getMaximumValueSize(boolean lossless)
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

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#canBeLossy()
	 */
	@Override
	public boolean canBeLossy()
	{
		return false;
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
		hash = 31 * hash + Objects.hashCode(rangeMapping);
		return hash;
	}

	@Override
	public Class<Long> getType()
	{
		return Long.class;
	}
	
}
