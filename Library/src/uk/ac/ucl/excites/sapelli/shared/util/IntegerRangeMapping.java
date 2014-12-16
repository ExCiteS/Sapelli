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

package uk.ac.ucl.excites.sapelli.shared.util;

import java.io.IOException;
import java.io.Serializable;
import java.math.BigInteger;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;

/**
 * Helper class that maps values from an integer range [x, y] (inclusive!), with x and y any signed 64 bit integers (i.e. longs),
 * and x < y (or x <= y when constructor argument {@code allowEmpty} is {@code true}), onto a range of positive (unsigned) (Big)integers [0, m]
 * 
 * The class computers a shift value to do the conversion as well as the numbers of bits needed to store any value from [0, m]
 * 
 * Integers of [x, y] are called logical values (to be used by client code), Integers of [0, m] are called raw values (to be used in underlying storage)
 * 
 * @author mstevens
 */
public class IntegerRangeMapping implements Serializable
{

	private static final long serialVersionUID = 2L;
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = {@code loBound} and y = (2^{@code sizeBits} - 1) + {@code loBound}.
	 * 
	 * @param loBound
	 * @param sizeBits
	 * @return
	 */
	static public IntegerRangeMapping ForSize(long loBound, int sizeBits)
	{
		if(sizeBits > Long.SIZE)
			throw new IllegalArgumentException("Maximum size is " + Long.SIZE + "bits, requested size was " + sizeBits + " bits.");
		return new IntegerRangeMapping(loBound, BigInteger.valueOf(2).pow(sizeBits).add(BigInteger.valueOf(-1)).add(BigInteger.valueOf(loBound)).longValue());
	}
	
	/**
	 * Number of bits needed to store values in the range.
	 */
	private final int size;
	
	/**
	 * The lower bound of the range,
	 * acts as the "shift" value which is subtracted to map values from the logical range to the raw range.
	 */
	private final long loBound;
	
	/**
	 * The strict upper bound of the range. 
	 * 
	 * @see #highBound(boolean)
	 */
	private final long hiBound;
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = 0 and y = {@code hiBound}.
	 * 
	 * @param hiBound
	 */
	public IntegerRangeMapping(long hiBound)
	{
		this(0, hiBound);
	}
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = {@code loBound} and y = {@code hiBound}.
	 * 
	 * @param loBound
	 * @param hiBound must be strictly larger than loBound
	 */
	public IntegerRangeMapping(long loBound, long hiBound)
	{	
		this(loBound, hiBound, false);
	}
	
	/**
	 * @param loBound
	 * @param hiBound
	 * @param allowEmpty
	 */
	public IntegerRangeMapping(long loBound, long hiBound, boolean allowEmpty)
	{	
		if(allowEmpty ? hiBound < loBound : hiBound <= loBound)
			throw new IllegalArgumentException("hiBound (" + hiBound + ") must be " + (allowEmpty ? "" : "strictly ") + "larger than " + (allowEmpty ? "or equal to " : "") + "lowBound (" + loBound + ")");
		this.loBound = loBound; // "shift" value
		this.hiBound = hiBound;
		BigInteger max = BigInteger.valueOf(hiBound).subtract(BigInteger.valueOf(loBound));
		size = max.bitLength(); // will be 0 if loBound = hiBound (only allowed when allowEmpty = true)
		// Without BigInteger: size = Long.SIZE - Long.numberOfLeadingZeros(max); //gets the numbers of bits needed to store a positive non-0 integer (log2(x))
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param another - a {@link IntegerRangeMapping} instance to be copied
	 */
	public IntegerRangeMapping(IntegerRangeMapping another)
	{
		this(another.loBound, another.hiBound, true);
	}
	
	/**
	 * @return the number of bits needed to store values in this range
	 */
	public int size()
	{
		return size;
	}
	
	/**
	 * The number of distinct values that can fit in this range
	 * 
	 * @return
	 */
	public BigInteger numberOfPossibleValues()
	{
		return numberOfPossibleValues(true);
	}
	
	/**
	 * The number of distinct values that can fit in this range
	 * 
	 * @param strict indicates whether the specified (true) or effective (false) upper bound is used
	 * @return
	 */
	public BigInteger numberOfPossibleValues(boolean strict)
	{
		return BigInteger.valueOf(highBound(strict)).subtract(BigInteger.valueOf(loBound)).add(BigInteger.valueOf(1l));
	}
	
	/**
	 * Returns the lower bound (inclusive!) of the logical range
	 * 
	 * @return the loBound (inclusive!)
	 */
	public long lowBound()
	{
		return loBound;
	}

	/**
	 * Returns the strict upper bound (inclusive!) of the logical range
	 * 
	 * @return the hiBound (inclusive!)
	 */
	public long highBound()
	{
		return highBound(true);
	}
	
	/**
	 * Returns the upper bound (inclusive!) of the logical range 
	 * 
	 * Meaning of the {@code strict} argument:
	 * 	- if {@code true} the logical range is taken as specified upon construction: [loBound, hiBound];
	 * 	- if {@code false} the logical range is taken as "effective" logical range of [loBound, (2^size) - 1 + loBound].
	 * 	The difference between the "strict" (or "as specified") logical range and the "effective" one stems from the
	 * 	fact that it is often "technically" possible to store higher logical values than the specified hiBound using
	 * 	the allocated number of bits (= size), namely all values from [loBound, (2^size) - 1 + loBound].
	 * 	For instance the logical range [-1, 255] will be mapped on [0, 256] but since this raw range requires 9 bits,
	 * 	we could actually store all logical values of [-1, 510] (mapped onto raw range of [0, 511]).
	 * 
	 * @param strict indicates whether the strict/specified (pass {@code true}) or effective (pass {@code false}) upper bound is returned
	 * @return the hiBound (inclusive!)
	 */
	public long highBound(boolean strict)
	{
		if(strict)
			return hiBound;
		else
			return BigInteger.valueOf(2).pow(size).subtract(BigInteger.ONE).add(BigInteger.valueOf(loBound)).longValue();
	}

	/**
	 * This method checks if the given value is in the logical range as specified upon construction: [loBound, hiBound].
	 *  
	 * @param logicalValue
	 * @return whether or not the logical value is in the range as specified upon construction (i.e. the "strict" range)
	 * 
	 * @see #highBound(boolean)
	 */
	public boolean inStrictRange(long logicalValue)
	{
		return inRange(logicalValue, true);
	}
	
	/**
	 * This method checks whether the given value is in the "effective" logical range of [loBound, (2^size) - 1 + loBound]
	 * 
	 * @param logicalValue
	 * @return whether or not the logical value is in the "effective" [loBound, (2^size) - 1 + loBound] range
	 * 
	 * @see #highBound(boolean)
	 */
	public boolean inEffectiveRange(long logicalValue)
	{
		return inRange(logicalValue, false);
		/*//Alternative implementation:
		BigInteger raw = toRawValue(logicalValue);
		return raw.signum() >= 0 && raw.bitLength() <= size; */
	}
	
	/**
	 * This method checks if the given value is in the logical range
	 * 
	 * @param logicalValue
	 * @param strict whether to use the strict/specified logical range (pass {@code true}), or the effective logical range (pass {@code false})
	 * @return whether or not the logical value is in the valid range
	 * 
	 * @see #highBound(boolean)
	 */
	public boolean inRange(long logicalValue, boolean strict)
	{
		return (lowBound() <= logicalValue) && (logicalValue <= highBound(strict));
	}
	
	/**
	 * @return a String describing the strict (as specified) range of allowed logical values
	 * 
	 * @see #highBound(boolean)
	 */
	public String getStrictRangeString()
	{
		return getRangeString(true);
	}
	
	/**
	 * @return a String describing the effective range of allowed logical values
	 * 
	 * @see #highBound(boolean)
	 */
	public String getEffectiveRangeString()
	{
		return getRangeString(false);
	}
	
	/**
	 * @param strict whether to use the strict/specified logical range (pass {@code true}), or the effective logical range (pass {@code false})
	 * @return a String describing the range of allowed logical values
	 * 
	 * @see #highBound(boolean)
	 */
	public String getRangeString(boolean strict)
	{
		return "[" + loBound + ", " + highBound(strict) + "]";
	}
	
	public BigInteger toRawValue(long logicalValue)
	{
		return BigInteger.valueOf(logicalValue).subtract(BigInteger.valueOf(loBound));
	}
	
	public long toLogicalValue(BigInteger rawValue)
	{
		return rawValue.add(BigInteger.valueOf(loBound)).longValue();
	}
	
	/**
	 * @param logicalValue
	 * @param to
	 * @throws IllegalArgumentException in case the {@code logicalValue} does not fit in the effective logical range
	 * @throws IOException in case of an I/O problem
	 */
	public void write(long logicalValue, BitOutputStream to) throws IllegalArgumentException, IOException
	{
		if(!inRange(logicalValue, false))
			throw new IllegalArgumentException("Logical value (" + logicalValue + ") does not fit in effective logical range: " + getRangeString(false));
		to.write(toRawValue(logicalValue), size, false);
	}
	
	/**
	 * @param from
	 * @return
	 * @throws IOException in case of an I/O problem
	 */
	public long read(BitInputStream from) throws IOException
	{
		return toLogicalValue(from.readBigInteger(size, false));
	}

	/**
	 * @param array
	 * @param offset
	 * @return
	 * @throws IOException in case of an I/O problem
	 */
	public long read(BitArray array, int offset) throws IOException
	{
		BitArrayInputStream stream = new BitArrayInputStream(array);
		try
		{
			stream.skipBits(offset);
			return read(stream);
		}
		finally
		{
			stream.close();
		}
	}
	
	public String toString()
	{
		BigInteger max = BigInteger.valueOf(hiBound).subtract(BigInteger.valueOf(loBound));
		return "IntegerRangeMapping of logical range " + getStrictRangeString() +
				" to raw range [0, " + max + "] (shift: " + loBound + "; size: " + size + " bits" +
				(highBound(true) != highBound(false) ? "; effective logical range: " + getEffectiveRangeString() : "") + ")";
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof IntegerRangeMapping)
		{
			IntegerRangeMapping that = (IntegerRangeMapping) obj;
			return this.loBound == that.loBound && this.hiBound == that.hiBound;
			// no need to check size as it depends only on loBound & hiBound
		}
		else
			return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (int)(loBound ^ (loBound >>> Integer.SIZE));
		hash = 31 * hash + (int)(hiBound ^ (hiBound >>> Integer.SIZE));
		// no need to check size as it depends only on loBound & hiBound
		return hash;
	}
	
}
