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

	// STATICS ----------------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	/**
	 * @param min (inclusive)
	 * @param max (inclusive)
	 * @return the String "[min, max]"
	 */
	static public String GetRangeString(long min, long max)
	{
		return GetRangeString(BigInteger.valueOf(min), BigInteger.valueOf(max));
	}
	
	/**
	 * @param min (inclusive)
	 * @param max (inclusive)
	 * @return the String "[min, max]"
	 */
	static public String GetRangeString(BigInteger min, BigInteger max)
	{
		return "[" + min + ", " + max + "]";
	}
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = {@code loBound} and y = (2^{@code sizeBits} - 1) + {@code loBound}.
	 * 
	 * @param loBound
	 * @param sizeBits must be >= 1
	 * @return
	 */
	static public IntegerRangeMapping ForSize(long loBound, int sizeBits)
	{
		return ForSize(BigInteger.valueOf(loBound), sizeBits, false);
	}
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = {@code loBound} and y = (2^{@code sizeBits} - 1) + {@code loBound}.
	 * 
	 * @param loBound
	 * @param sizeBits {@code sizeBits} must be >= 1, unless {@code allowEmpty} is {@code true}, in which case it must be >= 0
	 * @param allowEmpty
	 * @return
	 */
	static public IntegerRangeMapping ForSize(long loBound, int sizeBits, boolean allowEmpty)
	{
		return ForSize(BigInteger.valueOf(loBound), sizeBits, allowEmpty);
	}
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = {@code loBound} and y = (2^{@code sizeBits} - 1) + {@code loBound}.
	 * 
	 * @param loBound
	 * @param sizeBits {@code sizeBits} must be >= 1, unless {@code allowEmpty} is {@code true}, in which case it must be >= 0
	 * @param allowEmpty
	 * @return
	 */
	static public IntegerRangeMapping ForSize(BigInteger loBound, int sizeBits, boolean allowEmpty)
	{
		if(sizeBits < (allowEmpty ? 0 : 1))
			throw new IllegalArgumentException("Number of bits must not be less than " + (allowEmpty ? 0 : 1));
		return new IntegerRangeMapping(loBound, BigIntegerUtils.GetMaxValue(sizeBits, false).add(loBound), allowEmpty);
	}

	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!), with x and y taken respectively as the minimum and maximum
	 * signed (if {@code signed} is {@code true}) or unsigned (if it is {@code false}) integer that can be stored in {@code sizeBits} bits.
	 * 
	 * @param sizeBits must be >= 1
	 * @param signed
	 * @return
	 */
	static public IntegerRangeMapping ForSize(int sizeBits, boolean signed)
	{
		return ForSize(sizeBits, signed, false);
	}
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!), with x and y taken respectively as the minimum and maximum
	 * signed (if {@code signed} is {@code true}) or unsigned (if it is {@code false}) integer that can be stored in {@code sizeBits} bits.
	 * 
	 * @param sizeBits {@code sizeBits} must be >= 1, unless {@code allowEmpty} is {@code true}, in which case it must be >= 0
	 * @param signed
	 * @param allowEmpty
	 * @return
	 */
	static public IntegerRangeMapping ForSize(int sizeBits, boolean signed, boolean allowEmpty)
	{
		if(sizeBits < (allowEmpty ? 0 : 1))
			throw new IllegalArgumentException("Number of bits must not be less than " + (allowEmpty ? 0 : 1));
		return new IntegerRangeMapping(	BigIntegerUtils.GetMinValue(sizeBits, signed),
										BigIntegerUtils.GetMaxValue(sizeBits, signed), allowEmpty);
	}
	
	// DYNAMICS ---------------------------------------------------------------
	/**
	 * Number of bits needed to store values in the range.
	 */
	private final int size;
	
	/**
	 * The lower bound of the range,
	 * acts as the "shift" value which is subtracted to map values from the logical range to the raw range.
	 */
	private final BigInteger loBound;
	
	/**
	 * The strict upper bound of the range. 
	 * 
	 * @see #highBound(boolean)
	 */
	private final BigInteger hiBound;
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = 0 and y = {@code hiBound}.
	 * 
	 * @param hiBound
	 */
	public IntegerRangeMapping(long hiBound)
	{
		this(BigIntegerUtils.ZERO, BigInteger.valueOf(hiBound), false);
	}
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = 0 and y = {@code hiBound}.
	 * 
	 * @param hiBound
	 */
	public IntegerRangeMapping(BigInteger hiBound)
	{
		this(BigIntegerUtils.ZERO, hiBound, false);
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
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = {@code loBound} and y = {@code hiBound}.
	 * 
	 * @param loBound
	 * @param hiBound
	 * @param allowEmpty when {@code false} loBound must be < hiBound, when {@code true} loBound must be <= hiBound
	 */
	public IntegerRangeMapping(long loBound, long hiBound, boolean allowEmpty)
	{
		this(BigInteger.valueOf(loBound), BigInteger.valueOf(hiBound), allowEmpty);
	}
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = {@code loBound} and y = {@code hiBound}.
	 * 
	 * @param loBound
	 * @param hiBound must be strictly larger than loBound
	 */
	public IntegerRangeMapping(BigInteger loBound, BigInteger hiBound)
	{
		this(loBound, hiBound, false);
	}
	
	/**
	 * Creates an {@link IntegerRangeMapping} [x, y] (inclusive!) with x = {@code loBound} and y = {@code hiBound}.
	 * 
	 * @param loBound
	 * @param hiBound
	 * @param allowEmpty when {@code false} loBound must be < hiBound, when {@code true} loBound must be <= hiBound
	 */
	public IntegerRangeMapping(BigInteger loBound, BigInteger hiBound, boolean allowEmpty)
	{	
		if(allowEmpty ? hiBound.compareTo(loBound) < 0 : hiBound.compareTo(loBound) <= 0)
			throw new IllegalArgumentException("hiBound (" + hiBound + ") must be " + (allowEmpty ? "" : "strictly ") + "larger than " + (allowEmpty ? "or equal to " : "") + "lowBound (" + loBound + ")");
		this.loBound = loBound; // "shift" value
		this.hiBound = hiBound;
		BigInteger max = hiBound.subtract(loBound);
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
		return strict ?	hiBound.subtract(loBound).add(BigIntegerUtils.ONE) :
						BigIntegerUtils.NumberOfPossibleValues(size);
	}
	
	/**
	 * Returns the lower bound (inclusive!) of the logical range
	 * 
	 * @return the loBound (inclusive!)
	 */
	public BigInteger lowBound()
	{
		return loBound;
	}

	/**
	 * Returns the strict upper bound (inclusive!) of the logical range
	 * 
	 * @return the hiBound (inclusive!)
	 */
	public BigInteger highBound()
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
	public BigInteger highBound(boolean strict)
	{
		if(strict)
			return hiBound;
		else
			return BigIntegerUtils.TWO.pow(size).subtract(BigIntegerUtils.ONE).add(loBound);
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
		return raw.signum() >= 0 && raw.bitLength() <= size;*/
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
		return inRange(BigInteger.valueOf(logicalValue), strict);
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
	public boolean inRange(BigInteger logicalValue, boolean strict)
	{
		return (lowBound().compareTo(logicalValue) <= 0) && (logicalValue.compareTo(highBound(strict)) <= 0);
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
		return GetRangeString(loBound, highBound(strict));
	}
	
	public BigInteger toRawValue(long logicalValue)
	{
		return toRawValue(BigInteger.valueOf(logicalValue));
	}
	
	public BigInteger toRawValue(BigInteger logicalValue)
	{
		return logicalValue.subtract(loBound);
	}
	
	public BigInteger toLogicalValue(BigInteger rawValue)
	{
		return rawValue.add(loBound);
	}
	
	/**
	 * @param logicalValue
	 * @param to
	 * @throws IllegalArgumentException in case the {@code logicalValue} does not fit in the effective logical range
	 * @throws IOException in case of an I/O problem
	 */
	public void write(long logicalValue, BitOutputStream to) throws IllegalArgumentException, IOException
	{
		write(BigInteger.valueOf(logicalValue), to);
	}

	/**
	 * @param logicalValue
	 * @param to
	 * @throws IllegalArgumentException in case the {@code logicalValue} does not fit in the effective logical range
	 * @throws IOException in case of an I/O problem
	 */
	public void write(BigInteger logicalValue, BitOutputStream to) throws IllegalArgumentException, IOException
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
	public BigInteger read(BitInputStream from) throws IOException
	{
		return toLogicalValue(from.readBigInteger(size, false));
	}
	
	/**
	 * @param from
	 * @return
	 * @throws IOException
	 */
	public long readLong(BitInputStream from) throws IOException
	{
		return read(from).longValue();
	}
	
	/**
	 * @param from
	 * @return
	 * @throws IOException
	 */
	public int readInt(BitInputStream from) throws IOException
	{
		return read(from).intValue();
	}
	
	/**
	 * @param from
	 * @return
	 * @throws IOException
	 */
	public short readShort(BitInputStream from) throws IOException
	{
		return read(from).shortValue();
	}
	
	public String toString()
	{
		BigInteger rawMax = hiBound.subtract(loBound);
		return "IntegerRangeMapping of logical range " + getStrictRangeString() +
				" to raw range " + GetRangeString(BigIntegerUtils.ZERO, rawMax) + " (shift: " + loBound + "; size: " + size + " bits" +
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
			return this.loBound.compareTo(that.loBound) == 0 && this.hiBound.compareTo(that.hiBound) == 0;
			// no need to check size as it depends only on loBound & hiBound
		}
		else
			return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + loBound.hashCode();
		hash = 31 * hash + hiBound.hashCode();
		// no need to check size as it depends only on loBound & hiBound
		return hash;
	}
	
}
