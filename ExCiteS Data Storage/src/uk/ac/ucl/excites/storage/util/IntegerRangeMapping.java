/**
 * 
 */
package uk.ac.ucl.excites.storage.util;

import java.io.IOException;
import java.math.BigInteger;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;

/**
 * Helper class that maps values from an integer range [x, y],
 * with x and y any signed 64 bit integers (i.e. longs), and x < y, onto
 * a range of positive (unsigned) (Big)integers [0, m]
 * 
 * The class computers a shift value to do the conversion as well
 * as the numbers of bits needed to store any value from [0, m]
 * 
 * Integers of [x, y] are called logical values (to be used by higher level program)
 * Integers of [0, m] are called raw values (to be used in binary storage)
 * 
 * @author mstevens
 */
public class IntegerRangeMapping
{

	private int size; //size in number of bits
	private long loBound = 0; //"shift" value
	private long hiBound;
	
	public IntegerRangeMapping(long hiBound)
	{
		this(0, hiBound);
	}
	
	public IntegerRangeMapping(long loBound, long hiBound)
	{	
		if(hiBound <= loBound)
			throw new IllegalArgumentException("hiBound (" + hiBound + ") must be strictly larger than lowBound (" + loBound + ")");
		this.loBound = loBound; //"shift" value
		this.hiBound = hiBound;
		BigInteger max = BigInteger.valueOf(hiBound).subtract(BigInteger.valueOf(loBound));
		size = max.bitLength();
		//Without BigInteger: size = Long.SIZE - Long.numberOfLeadingZeros(max); //gets the numbers of bits needed to store a positive non-0 integer (log2(x))
		
		System.out.println("[" + loBound + ", " + hiBound + "] will be mapped on: [ 0, " + max + " ]; Shift: " + loBound + "; Bits needed: " + size);
	}
	
	/**
	 * @return the number of bits needed to store values in this range
	 */
	public int getSize()
	{
		return size;
	}
	
	/**
	 * @return the loBound
	 */
	public long getLowBound()
	{
		return loBound;
	}

	/**
	 * @return the hiBound
	 */
	public long getHighBound()
	{
		return hiBound;
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
	 * This method checks if the given value is in the logical range specified upon construction ([loBound, hiBound]).
	 * 
	 * However, often it will be "technically" possible to store logical higher values using the allocated number
	 * of bits (= size), namely all values from [loBound, (2^size) - 1 + loBound].
	 * For instance the logical range [-1, 255] will be mapped on [0, 256] but since this raw range requires 9 bits,
	 * we could actually store all logical values of [-1, 510] (mapped onto raw range of [0, 511]).
	 * To check if a logical value falls within this "effective" logical range the fits(long) method should be used instead.
	 * 
	 * @param logicalValue
	 * @return whether or not the logical value is in the valid [loBound, hiBound] range
	 */
	public boolean inRange(long logicalValue)
	{
		return (loBound <= logicalValue) && (logicalValue <= hiBound);
	}
	
	/**
	 * This method checks whether the given value is in the "effective" logical range of [loBound, (2^size) - 1 + loBound]
	 * 
	 * To check if the value is in the logical range specified upon construction ([loBound, hiBound]) the inRange(long) method should be used instead.
	 * 
	 * @param logicalValue
	 * @return whether or not the logical value is in the "effective" [loBound, (2^size) - 1 + loBound] range
	 */
	public boolean fits(long logicalValue)
	{
		BigInteger raw = toRawValue(logicalValue);
		return raw.signum() >= 0 && raw.bitLength() <= size;
	}
	
}
