/**
 * 
 */
package uk.ac.ucl.excites.transmission.lib.util;

import java.io.IOException;

import uk.ac.ucl.excites.transmission.lib.io.BitInputStream;
import uk.ac.ucl.excites.transmission.lib.io.BitOutputStream;

/**
 * Helper class that maps values from an integer range [x, y],
 * with x and y any signed integers (i.e. ints), and x < y, onto
 * a range of positive (unsigned) integers [0, m]
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
	private long shift = 0;
	
	public IntegerRangeMapping(int hiBound)
	{
		this(0, hiBound);
	}
	
	public IntegerRangeMapping(int lowBound, int hiBound)
	{	
		if(hiBound <= lowBound)
			throw new IllegalArgumentException("hiBound (" + hiBound + ") must be strictly larger than lowBound (" + lowBound + ")");
		//System.out.println("Integer range: [ " + lowBound + ", " + hiBound + " ]");
		shift = lowBound;
		long max = ((long) hiBound) - ((long) lowBound);
		size = Long.SIZE - Long.numberOfLeadingZeros(max); //gets the numbers of bits needed to store a positive non-0 integer (log2(x))
		
		System.out.println("Will be stored as: [ 0, " + max + " ]; Shift: " + shift);
		System.out.println("Bits needed: " + size);
	}
	
	public int getSize()
	{
		return size;
	}
	
	public long getShiftValue()
	{
		return shift;
	}
	
	public long toRawValue(int logicalValue)
	{
		return ((long) logicalValue) - shift;
	}
	
	public int toLogicalValue(long rawValue)
	{
		return (int) (rawValue + shift);
	}
	
	public void write(int logicalValue, BitOutputStream to) throws IOException
	{
		to.write(toRawValue(logicalValue), size, false);
	}
	
	public int read(BitInputStream from) throws IOException
	{
		return toLogicalValue(from.readInteger(size, false));
	}
	
}
