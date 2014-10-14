/**
 * 
 */
package uk.ac.ucl.excites.sapelli.shared.io;

import java.io.IOException;
import java.util.BitSet;

/**
 * @author mstevens
 *
 */
public class BitArrayOutputStream extends BitOutputStream
{

	private BitSet bits;
	
	/**
	 * @param out
	 */
	public BitArrayOutputStream()
	{
		super();
		this.bits = new BitSet();
	}

	@Override
	protected void writeBit(boolean bit) throws IOException
	{
		bits.set(numberOfBitsWritten, bit);
	}
	
	public BitArray toBitArray()
	{
		return new BitArray(bits, numberOfBitsWritten);
	}
	
}
