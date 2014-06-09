package uk.ac.ucl.excites.sapelli.shared.io;


import java.io.IOException;
import java.io.OutputStream;


/**
 * A stream where bits can be written to. Provides write methods for various (primitive) types.<br/>
 * <br/>
 * Heavily modified/extended version of original work by Nayuki Minase:<br/>
 * 		- Source: <a href="https://github.com/nayuki/Huffman-Coding/blob/master/src/nayuki/huffmancoding/BitOutputStream.java">https://github.com/nayuki/Huffman-Coding/blob/master/src/nayuki/huffmancoding/BitOutputStream.java</a><br/>
 * 		- License: MIT License<br/>
 * 
 * @author mstevens
 */
public final class BitWrapOutputStream extends BitOutputStream
{
	
	//DYNAMIC
	private OutputStream output; 		// Underlying byte stream to write to
	private int currentByte; 			// Buffered bits stored as an int (always in the range 0x00 to 0xFF)
	private int numBitsInCurrentByte; 	// Always between 0 and 7, inclusive
	
	public BitWrapOutputStream(OutputStream out)
	{
		super(); //!!!
		if(out == null)
			throw new NullPointerException("Underlying OutputStream cannot be null!");
		output = out;
		currentByte = 0;
		numBitsInCurrentByte = 0;
	}

	/**
	 * Writes an individual bit (a boolean) to the output
	 * 
	 * @param bit bit (true = 1; false = 0) to be written
	 * @throws IOException if an I/O error occurs
	 */
	protected void writeBit(boolean bit) throws IOException
	{
		currentByte <<= 1;
		if(bit)
			currentByte++;
		numBitsInCurrentByte++;
		if(numBitsInCurrentByte == 8)
		{
			output.write(currentByte);
			currentByte = 0;
			numBitsInCurrentByte = 0;
		}
	}
	
	/**
	 * Closes this stream and the underlying OutputStream.
	 * If called when this bit stream is not at a byte boundary, then the minimum number of zeros (between 0 and 7) are written as padding to reach a byte boundary.
	 * 
	 * @throws IOException if an I/O error occurs
	 * @see java.io.OutputStream#close()
	 */
	public void close() throws IOException
	{
		if(!closed)
		{
			writePadding();
			output.close();
			super.close();
		}
	}
	
	/**
	 * Write zeros (=false) as padding until a byte boundary is reached
	 * 
	 * @throws IOException if an I/O error occurs
	 */
	private void writePadding() throws IOException
	{
		while(numBitsInCurrentByte != 0)
			write(false);
	}

    /**
     * Flushes this and the underlying output stream and forces any buffered bits to be written out.
     * 
     * @throws IOException if an I/O error occurs
     * @see java.io.OutputStream#flush()
     */
    public void flush() throws IOException
    {
    	writePadding();
    	output.flush();
    	super.flush();
    }
    
    public int getNumberOfBitsWritten()
    {
    	return numberOfBitsWritten;
    }

}
