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

	static public final int UNLIMITED = -1; 
	
	private final BitSet bits;
	private final int maxLength;
	
	/**
	 * @param out
	 */
	public BitArrayOutputStream()
	{
		this(UNLIMITED);
	}

	/**
	 * @param out
	 */
	public BitArrayOutputStream(int maxLength)
	{
		super();
		if(maxLength < UNLIMITED)
			throw new IllegalArgumentException("maxLength cannot be < -1");
		this.bits = new BitSet();
		this.maxLength = maxLength;
	}
	
	/**
	 * Writes an individual bit (a boolean) to the underlying BitSet
	 * 
	 * @param bit bit (true = 1; false = 0) to be written
	 * @throws IOException if an I/O error occurs
	 */
	@Override
	protected void writeBit(boolean bit) throws IOException
	{
		bits.set(getNumberOfBitsWritten(), bit);
	}
	
	/**
	 * @return
	 */
	public BitArray toBitArray()
	{
		return toBitArray(false);
	}
	
	/**
	 * @param useMaxLenth
	 * @return
	 */
	public BitArray toBitArray(boolean useMaxLenth)
	{
		return new BitArray(bits, useMaxLenth && isLimited() ? maxLength : getNumberOfBitsWritten());
	}

	/**
	 * @return the maxLength
	 */
	public int getMaxLength()
	{
		return maxLength;
	}
	
	/**
	 * @return
	 */
	public boolean isLimited()
	{
		return maxLength > UNLIMITED;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream#isFull()
	 */
	public boolean isFull()
	{
		return isLimited() && getNumberOfBitsWritten() == maxLength;
	}
	
}
