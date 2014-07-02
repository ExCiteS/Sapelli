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

package uk.ac.ucl.excites.sapelli.storage.util;

import java.io.IOException;
import java.io.Serializable;
import java.math.BigInteger;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;

/**
 * Helper class that maps values from an integer range [x, y] (inclusive!),
 * with x and y any signed 64 bit integers (i.e. longs), and x < y, onto
 * a range of positive (unsigned) (Big)integers [0, m]
 * 
 * The class computers a shift value to do the conversion as well
 * as the numbers of bits needed to store any value from [0, m]
 * 
 * Integers of [x, y] are called logical values (to be used by higher level program),
 * Integers of [0, m] are called raw values (to be used in underlying storage)
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
	
	private int size; //size in number of bits
	private long loBound = 0; //"shift" value
	private long hiBound;
	
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
	 * @param hiBound
	 */
	public IntegerRangeMapping(long loBound, long hiBound)
	{	
		if(hiBound <= loBound)
			throw new IllegalArgumentException("hiBound (" + hiBound + ") must be strictly larger than lowBound (" + loBound + ")");
		this.loBound = loBound; //"shift" value
		this.hiBound = hiBound;
		BigInteger max = BigInteger.valueOf(hiBound).subtract(BigInteger.valueOf(loBound));
		size = max.bitLength();
		//Without BigInteger: size = Long.SIZE - Long.numberOfLeadingZeros(max); //gets the numbers of bits needed to store a positive non-0 integer (log2(x))
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param another	a {@link IntegerRangeMapping} instance to be copied
	 */
	public IntegerRangeMapping(IntegerRangeMapping another)
	{
		this.size = another.size;
		this.loBound = another.loBound;
		this.hiBound = another.hiBound;
	}
	
	/**
	 * @return the number of bits needed to store values in this range
	 */
	public int getSize()
	{
		return size;
	}
	
	/**
	 * @return the loBound (inclusive!)
	 */
	public long getLowBound()
	{
		return loBound;
	}

	/**
	 * @return the hiBound (inclusive!)
	 */
	public long getHighBound()
	{
		return getHighBound(true);
	}
	
	/**
	 * @param strict indicates whether the specified (true) or effective (false) upper bound is returned
	 * @return the hiBound (inclusive!)
	 */
	public long getHighBound(boolean strict)
	{
		if(strict)
			return hiBound;
		else
			return BigInteger.valueOf(2).pow(size).subtract(BigInteger.ONE).add(BigInteger.valueOf(loBound)).longValue();
	}

	public BigInteger toRawValue(long logicalValue)
	{
		return BigInteger.valueOf(logicalValue).subtract(BigInteger.valueOf(loBound));
	}
	
	public long toLogicalValue(BigInteger rawValue)
	{
		return rawValue.add(BigInteger.valueOf(loBound)).longValue();
	}
	
	public void write(long logicalValue, BitOutputStream to) throws IOException
	{
		to.write(toRawValue(logicalValue), size, false);
	}
	
	public long read(BitInputStream from) throws IOException
	{
		return toLogicalValue(from.readBigInteger(size, false));
	}

	/**
	 * This method checks if the given value is in the logical range
	 * 
	 * If strict is true the logical range is taken as specified upon construction: [loBound, hiBound];
	 * If strict is false the logical range is taken as "effective" logical range of [loBound, (2^size) - 1 + loBound].
	 * 
	 * The difference between the logical range as specified and the "effective" one stems from the fact that it is
	 * often "technically" possible to store higher logical values than the specified hiBound using the allocated
	 * number of bits (= size), namely all values from [loBound, (2^size) - 1 + loBound].
	 * For instance the logical range [-1, 255] will be mapped on [0, 256] but since this raw range requires 9 bits,
	 * we could actually store all logical values of [-1, 510] (mapped onto raw range of [0, 511]).
	 * 
	 * @param logicalValue
	 * @parem strict whether or not to use the specified or effective logical range
	 * @return whether or not the logical value is in the valid range
	 */
	public boolean inRange(long logicalValue, boolean strict)
	{
		return (getLowBound() <= logicalValue) && (logicalValue <= getHighBound(strict));
	}

	/**
	 * This method checks if the given value is in the logical range as specified upon construction: [loBound, hiBound].
	 *  
	 * @param logicalValue
	 * @return whether or not the logical value is in the valid range as specified upon construction
	 */
	public boolean inRange(long logicalValue)
	{
		return inRange(logicalValue, true);
	}
	
	/**
	 * This method checks whether the given value is in the "effective" logical range of [loBound, (2^size) - 1 + loBound]
	 * 
	 * @param logicalValue
	 * @return whether or not the logical value is in the "effective" [loBound, (2^size) - 1 + loBound] range
	 */
	public boolean fits(long logicalValue)
	{
		return inRange(logicalValue, false);
		/*//Alternative implementation:
		BigInteger raw = toRawValue(logicalValue);
		return raw.signum() >= 0 && raw.bitLength() <= size; */
	}
	
	public String toString()
	{
		BigInteger max = BigInteger.valueOf(hiBound).subtract(BigInteger.valueOf(loBound));
		return "IntegerRangeMapping of [" + loBound + ", " + hiBound + "] to [0, " + max + "] (shift: " + loBound + "; size: " + size + " bits)";
	}
	
	public String getLogicalRangeString()
	{
		return "[" + loBound + ", " + hiBound + "]";
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof IntegerRangeMapping)
		{
			IntegerRangeMapping other = (IntegerRangeMapping) obj;
			return this.size == other.size && this.loBound == other.loBound && this.hiBound == other.hiBound;
		}
		else
			return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + size;
		hash = 31 * hash + (int)(loBound ^ (loBound >>> Integer.SIZE));
		hash = 31 * hash + (int)(hiBound ^ (hiBound >>> Integer.SIZE));
		return hash;
	}
	
}
