package uk.ac.ucl.excites.sapelli.shared.io;


import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

/**
 * A stream of bits that can be read. Provides read methods for various (primitive) types.<br/>
 * <br/>
 * Heavily modified/extended version of original work by Nayuki Minase:<br/>
 * 		- Source: <a href="https://github.com/nayuki/Huffman-Coding/blob/master/src/nayuki/huffmancoding/BitInputStream.java">https://github.com/nayuki/Huffman-Coding/blob/master/src/nayuki/huffmancoding/BitInputStream.java</a><br/>
 * 		- License: MIT License<br/>
 * 
 * @author mstevens
 */
public final class BitWrapInputStream extends BitInputStream
{
	
	//DYNAMIC
	private InputStream input;			// Underlying byte-based InputStream to read from
	private int currentByte;			// Buffered bits stored as an int (either in the range 0x00 to 0xFF, or -1 if the end of stream is reached)
	private int numBitsRemaining;		// Always between 0 and 7, inclusive
	private boolean isEndOfStream;

	public BitWrapInputStream(InputStream input)
	{
		if(input == null)
			throw new NullPointerException("Underlying InputStream cannot be null!");
		this.input = input;
		numBitsRemaining = 0;
		isEndOfStream = false;
	}
	
	/**
	 * @return whether stream is at end (true) or not (false)
	 * @throws IOException if the stream is closed or another I/O error occurs
	 */
	public boolean atEnd() throws IOException
	{
		if(closed)
			throw new IOException("This stream is closed");
		if(isEndOfStream && numBitsRemaining == 0)
			return true;
		if(numBitsRemaining == 0)
		{
			currentByte = input.read();
			if(currentByte == -1)
			{
				isEndOfStream = true;
				return true;
			}
			else
				numBitsRemaining = 8;
		}
		return false;
	}

	/**
	 * Reads a bit from the stream. Returns a boolean (true for 1; false for 0) if a bit is available,
	 * or throws an EOFException if the end of stream is reached.
	 * 
	 * @return the bit that was read (true = 1; false = 0)
	 * @throws IOException if the stream is closed or another I/O error occurs
	 * @throws EOFException when the next bit cannot be read because the end of stream is reached
	 */
	public boolean readBit() throws IOException, EOFException
	{
		if(atEnd()) //also reads a new byte from underlying stream if needed! (will also check for closedness)
			throw new EOFException("End of stream reached");
		numBitsRemaining--;
		return ((currentByte >>> numBitsRemaining) & 1) == 1;
	}
	
	/**
	 * Closes this stream and the underlying InputStream.
	 * 
	 * @throws IOException if an I/O error occurs
	 */
	public void close() throws IOException
	{
		if(!closed)
		{
			input.close();
			super.close();
		}
	}
	
	/**
	 * The (estimated) number of bits left available for reading.
	 * Calls atEnd().
	 * 
	 * @return
	 */
	public int bitsAvailable() throws IOException
	{
		try
		{
			if(atEnd())
				return 0;
			else
				return numBitsRemaining + (input.available() * 8);
		}
		catch(IOException e)
		{
			return numBitsRemaining;
		}
	}
	
	/* (non-Javadoc)
	 * @see java.io.InputStream#available()
	 */
	public int available() throws IOException
	{
		if(closed)
			throw new IOException("This stream is closed");
		return (numBitsRemaining == 8 ? 1 : 0) + input.available();
	}
		
}
