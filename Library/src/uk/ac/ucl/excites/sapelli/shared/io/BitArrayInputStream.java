package uk.ac.ucl.excites.sapelli.shared.io;


import java.io.EOFException;
import java.io.IOException;

/**
 * A stream of bits that can be read. Provides read methods for various (primitive) types.<br/>
 * <br/>
 * Heavily modified/extended version of original work by Nayuki Minase:<br/>
 * 		- Source: <a href="https://github.com/nayuki/Huffman-Coding/blob/master/src/nayuki/huffmancoding/BitInputStream.java">https://github.com/nayuki/Huffman-Coding/blob/master/src/nayuki/huffmancoding/BitInputStream.java</a><br/>
 * 		- License: MIT License<br/>
 * 
 * @author mstevens
 */
public final class BitArrayInputStream extends BitInputStream
{
	
	//DYNAMIC
	private BitArray bitArray;
	private int currentIndex;

	public BitArrayInputStream(BitArray bitArray)
	{
		super();
		this.bitArray = bitArray;
		currentIndex = 0;
	}

	/**
	 * Reads a bit from the stream. Returns a boolean (true for 1; false for 0) if a bit is available,
	 * or throws an EOFException if the end of stream is reached.
	 * 
	 * @return the bit that was read (true = 1; false = 0)
	 * @throws IOException if the stream is closed or another I/O error occurs
	 * @throws EOFException when the next bit cannot be read because the end of stream is reached
	 */
	protected boolean doReadBit() throws IOException, EOFException
	{
		if(currentIndex >= bitArray.length()) //also reads a new byte from underlying stream if needed! (will also check for closedness)
			throw new EOFException("End of stream reached");
		return bitArray.get(currentIndex++);
	}
	
	/**
	 * The (estimated) number of bits left available for reading.
	 * Calls atEnd().
	 * 
	 * @return
	 */
	public int bitsAvailable() throws IOException
	{
		return bitArray.length() - currentIndex;
	}
	
	/* (non-Javadoc)
	 * @see java.io.InputStream#available()
	 */
	public int available() throws IOException
	{
		return bitsAvailable() * Byte.SIZE;
	}
		
}
