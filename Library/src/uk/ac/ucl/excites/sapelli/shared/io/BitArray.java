/**
 * 
 */
package uk.ac.ucl.excites.sapelli.shared.io;

import java.util.BitSet;

import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;

/**
 * @author mstevens
 * 
 */
public class BitArray
{

	private BitSet bits;
	private int length;

	/**
	 * @param bits
	 * @param length
	 */
	public BitArray(BitSet bits, int length)
	{
		if(length < 0)
			throw new IllegalArgumentException("length cannot be negative!");
		this.bits = bits;
		this.length = length;
	}

	public boolean get(int index)
	{
		if(index >= 0 && index < length)
			return bits.get(index);
		throw new IndexOutOfBoundsException("index (" + index + ") out of bounds [0, " + (length - 1) + "]!");
	}

	public int length()
	{
		return length;
	}

	public byte[] toByteArray()
	{
		byte[] bytes = new byte[BinaryHelpers.bytesNeeded(length)];
		for(int i = 0; i < length; i++)
			if(bits.get(i))
				bytes[i / Byte.SIZE] |= 1 << (7 - (i % 8)); //MSB is read first
		return bytes;
	}

}
