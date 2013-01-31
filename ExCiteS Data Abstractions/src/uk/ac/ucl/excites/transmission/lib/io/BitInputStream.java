package uk.ac.ucl.excites.transmission.lib.io;


import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.nio.charset.Charset;

/**
 * A stream of bits that can be read. Provides read methods for various (primitive) types.<br/>
 * <br/>
 * Based on original work by Nayuki Minase:<br/>
 * 		- Source: <a href="https://github.com/nayuki/Huffman-Coding/blob/master/src/nayuki/huffmancoding/BitInputStream.java">https://github.com/nayuki/Huffman-Coding/blob/master/src/nayuki/huffmancoding/BitInputStream.java</a><br/>
 * 		- License: MIT License<br/>
 * 
 * @author mstevens
 */
public final class BitInputStream extends InputStream
{
	//STATIC
	private static final Charset DEFAULT_CHARSET = Charset.forName("UTF-8");
	private static final Charset UTF16BE = Charset.forName("UTF-16BE");
	
	//DYNAMIC
	private boolean closed;
	private InputStream input;			// Underlying byte stream to read from
	private int currentByte;			// Buffered bits stored as an int (either in the range 0x00 to 0xFF, or -1 if the end of stream is reached)
	private int numBitsRemaining;		// Always between 0 and 7, inclusive

	private boolean isEndOfStream;

	public BitInputStream(InputStream in)
	{
		if(in == null)
			throw new NullPointerException("Underlying InputStream cannot be null!");
		closed = false;
		input = in;
		numBitsRemaining = 0;
		isEndOfStream = false;
	}
	
	/**
	 * @return whether stream is at end (true) or not (false)
	 * @throws IOException if an I/O error occurs
	 */
	public boolean atEnd() throws IOException
	{
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
	 * @throws IOException if an I/O error occurs
	 * @throws EOFException could not read 8 bits
	 */
	public boolean readBit() throws IOException, EOFException
	{
		if(closed)
			throw new IOException("This stream is closed");
		if(atEnd()) //also reads a new byte from underlying stream if needed!
			throw new EOFException("End of stream reached");
		numBitsRemaining--;
		return ((currentByte >>> numBitsRemaining) & 1) == 1;
	}
	
	/**
	 * Reads a specified number of bits from the input stream
	 * 
	 * @param numberOfBits number of bits to be read
	 * @return boolean array with the bits that were read
	 * @throws IOException if an I/O error occurs
	 * @throws EOFException could not read enough bits
	 */
	public boolean[] readBits(int numberOfBits) throws EOFException, IOException
	{
		boolean[] bits = new boolean[numberOfBits];
		for(int i = 0; i < numberOfBits; i++)
			bits[i] = readBit();
		return bits;
	}
	
	/**
	 * Read exactly one byte from the input and returns it
	 * Throws an exception when less than 8 bits could be read
	 * 
	 * @return the byte
	 * @throws IOException if an I/O error occurs
	 * @throws EOFException could not read exactly 8 bits
	 */
	public byte readByte() throws IOException, EOFException
	{
		return readByte(true);
	}
	
	/**
	 * Reads a byte from the input
	 * If strict is true exactly 8 bits must be read, if it is false one bit is enough
	 * 
	 * The byte is assumed to be stored with MSB 0 bit order (i.e. the first bit read is the most significant one)
	 * 
	 * @param strict whether we want 8 bits to be read (true) or whether 1 is enough (false)
	 * @return the byte that was read
	 * @throws IOException
	 * @throws EOFException could not read enough bits (< 8 when strict = true; 0 when strict = false)
	 * @see <a href="http://en.wikipedia.org/wiki/Bit_numbering">http://en.wikipedia.org/wiki/Bit_numbering</a>
	 */
	public byte readByte(boolean strict) throws IOException, EOFException
	{
		if(closed)
			throw new IOException("This stream is closed");
		byte b = 0;
		int i = 0;
		try
		{
			for(; i < 8; i++)
				if(readBit())
					b |= 1 << (7 - i); //MSB is read first
		}
		catch(EOFException eof)
		{
			if(strict)
				throw new EOFException("Could not read whole byte");
			else if(i == 0)
				throw new EOFException("Could not read a single bit");
		}
		return b;
	}
	
	/* (non-Javadoc)
	 * @see java.io.InputStream#read(byte[])
	 */
	public int read(byte[] b) throws IOException
	{
		return read(b, 0, b.length);
	}
	
	/* (non-Javadoc)
	 * @see java.io.InputStream#read(byte[], int, int)
	 */
	public int read(byte[] b, int off, int len) throws IOException
	{
		if(b == null)
			throw new NullPointerException("Byte array cannot be null");
		if(off < 0 || len < 0 || off + len > b.length)
			throw new IndexOutOfBoundsException("Offset and/or length cause out of bounds exception");
		if(closed)
			throw new IOException("This stream is closed");
		int i = off;
		for(; i < len; i++)
		{
			try
			{
				b[i] = readByte(true);
			}
			catch(EOFException eof)
			{
				if(i == off)
					return -1; //not a single byte could be read
				break;
			}
		}
		return i - off; //number of bytes read
	}
	
	/**
	 * Reads exactly n whole bytes from the input
	 * 
	 * @param n number of bytes to read
	 * @return array of the bytes that were read
	 * @throws IOException if an I/O error occurs
	 * @throws EOFException could not read enough bytes
	 */
	public byte[] readBytes(int n) throws IOException, EOFException
	{
		return readBytes(n, true);
	}
	
	/**
	 * Read n bytes from the input
	 * If strict = true it throws an EOFException if not enough bytes where available.
	 * If strict = false it only throws an EOFException if not a single byte could be read
	 * 
	 * @param n number of bytes to read
	 * @param strict
	 * @return array of the bytes that were read
	 * @throws IOException if an I/O error occurs
	 * @throws EOFException could not read enough bytes (< n when strict = true; 0 when strict = false)
	 */
	public byte[] readBytes(int n, boolean strict) throws IOException, EOFException
	{
		if(n < 0)
			throw new IllegalArgumentException("Byte count cannot be negative");
		if(closed)
			throw new IOException("This stream is closed");
		byte[] bytes = new byte[n];
		int bytesRead = read(bytes);
		if(bytesRead < n && strict)
			throw new EOFException("Could not read enough bytes");
		if(bytesRead == -1)
			throw new EOFException("Could not read a single byte");
		return bytes;
	}
	
	/**
	 * Reads the next byte of data from the input stream.
	 *  
	 * Has the same semantics as InputStream.read():
	 * 		the value byte is returned as an int in the range 0 to 255;
	 * 		if no byte is available because the end of the stream has been reached, the value -1 is returned.
	 * 
	 * @see java.io.InputStream#read()
	 * 
	 * @return the next byte of data, or -1 if the end of the stream is reached
	 * @throws IOException if an I/O error occurs
	 */
	public int read() throws IOException
	{
		try
		{
			return (int) readInteger(Byte.SIZE, false);
		}
		catch(EOFException eof)
		{
			return -1;
		}
	}
	
	/**
	 * Reads a 16 bit signed integer (short) from the input
	 * 
	 * @return the value that was read
	 * @throws IOException if an I/O error occurs
	 */
	public short readShort() throws IOException
	{
		return (short) readInteger(Short.SIZE, true);
	}
	
	/**
	 * Reads a 32 bit signed integer (int) from the input
	 * 
	 * @return the value that was read
	 * @throws IOException if an I/O error occurs
	 */
	public int readInt() throws IOException
	{
		return (int) readInteger(Integer.SIZE, true);
	}
	
	/**
	 * Reads a 64 bit signed integer (long) from the input
	 * 
	 * @return the value that was read
	 * @throws IOException if an I/O error occurs
	 */
	public long readLong() throws IOException
	{
		return readInteger(Long.SIZE, true);
	}
	
	/**
 	 * Reads an integer value of specified number of bits (max 64) and specified "signedness" from the input and returns it as a long.
 	 * 
 	 * Signed values are assumed to be stored using Two's complement format.
	 * Currently only supports big-endian byte order (and MSB 0 bit numbering), meaning the more significant bits (and bytes) are read first.
	 * 
	 * @param numberOfBits the number of bits (> 0; <= 64 if signed; <= 63 if unsigned)
	 * @param signed the "signedness" (true = signed; false = unsigned)
	 * @return the value that was read
	 * @throws IOException if an I/O error occurs
	 * @see <a href="http://en.wikipedia.org/wiki/Integer_(computer_science)">http://en.wikipedia.org/wiki/Integer_(computer_science)</a>
	 * @see <a href="http://en.wikipedia.org/wiki/Two's_complement">http://en.wikipedia.org/wiki/Two's_complement</a>
	 * @see <a href="http://en.wikipedia.org/wiki/Endianness">http://en.wikipedia.org/wiki/Endianness</a>
	 * @see <a href="http://en.wikipedia.org/wiki/Bit_numbering">http://en.wikipedia.org/wiki/Bit_numbering</a>
	 */
	public long readInteger(int numberOfBits, boolean signed/*, ByteOrder order*/) throws IOException
	{
		//TODO add support for little-endian byte order (and perhaps LSB 0 bit numbering)
		if(numberOfBits > 64)
			throw new IllegalArgumentException("Cannot store more than 64 bits in a variable of type long; use readBigInteger() instead.");
		if(numberOfBits == 64 && !signed)
			throw new IllegalArgumentException("Cannot savely store unsigned values of more than 63 bits in a variable of type long; use readBigInteger() instead.");
		return readBigInteger(numberOfBits, signed).longValue();
	}
	
	/**
 	 * Reads an integer value of specified number of bits and specified "signedness" from the input and returns it as a BigInteger.
 	 * 
 	 * Signed values are assumed to be stored using Two's complement format.
	 * Currently only supports big-endian byte order (and MSB 0 bit numbering), meaning the more significant bits (and bytes) are read first.
	 * 
	 * @param numberOfBits the number of bits (> 0)
	 * @param signed the "signedness" (true = signed; false = unsigned)
	 * @return the value that was read
	 * @throws IOException if an I/O error occurs
	 * @see java.math.BigInteger
	 * @see <a href="http://en.wikipedia.org/wiki/Integer_(computer_science)">http://en.wikipedia.org/wiki/Integer_(computer_science)</a>
	 * @see <a href="http://en.wikipedia.org/wiki/Two's_complement">http://en.wikipedia.org/wiki/Two's_complement</a>
	 * @see <a href="http://en.wikipedia.org/wiki/Endianness">http://en.wikipedia.org/wiki/Endianness</a>
	 * @see <a href="http://en.wikipedia.org/wiki/Bit_numbering">http://en.wikipedia.org/wiki/Bit_numbering</a>
	 */
	public BigInteger readBigInteger(int numberOfBits, boolean signed/*, ByteOrder order*/) throws IOException
	{
		//TODO add support for little-endian byte order (and perhaps LSB 0 bit numbering)
		if(closed)
			throw new IOException("This stream is closed");
		if(numberOfBits < 1)
			throw new IllegalArgumentException("Invalid number of bits (" + numberOfBits + ").");
		//Determine min/max values:
		BigInteger minValue = signed ? 	BigInteger.valueOf(2).pow(numberOfBits - 1).negate() :
										BigInteger.ZERO;
		BigInteger maxValue = signed ? 	BigInteger.valueOf(2).pow(numberOfBits - 1).subtract(BigInteger.ONE) :
										BigInteger.valueOf(2).pow(numberOfBits).subtract(BigInteger.ONE);
		/*Read the value bit by bit
		 *	The most significant bit is read first ("MSB 0" bit numbering).
		 *	But we need to count backwards because BigInteger uses "LSB 0" bit numbering internally (i.e. the most significant bit is at address numburOfBits-1). */
		BigInteger value = BigInteger.ZERO;
		for(int i = numberOfBits - 1; i >= 0; i--)
			if(readBit())
				value = value.setBit(i);
			//Alternative for the 2 lines above:
			//	value = value.add(BigInteger.valueOf(readBit() ? (1l << i) : 0l));
		//Overflowing values become negative:
		if(value.compareTo(maxValue) > 0) //is value bigger than maxValue?
			value = minValue.add(value.subtract(maxValue).subtract(BigInteger.ONE));
		return value;
	}
	
	/**
	 * Reads a 32bit floating point value (a float) from the input
	 * 
	 * @return
	 * @throws IOException if an I/O error occurs
	 */
	public float readFloat() throws IOException
	{
		return Float.intBitsToFloat((int) readInteger(Float.SIZE, false));
	}
	
	/**
	 * Reads a 64bit floating point value (a double) from the input
	 * 
	 * @return
	 * @throws IOException if an I/O error occurs
	 */
	public double readDouble() throws IOException
	{
		return Double.longBitsToDouble(readInteger(Double.SIZE, false));
	}
	
	/**
	 * Reads a String composed of the specified number of bytes from the input,
	 * using the default character set (UTF-8).
	 * 
	 * @param numberOfBytes
	 * @return
	 * @throws IOException if an I/O error occurs
	 * @see <a href="http://en.wikipedia.org/wiki/UTF-8">http://en.wikipedia.org/wiki/UTF-8</a>
	 */
	public String readString(int numberOfBytes) throws IOException
	{
		return readString(numberOfBytes, DEFAULT_CHARSET);
	}
	
	/**
	 * Reads a String composed of the specified number of bytes from the input,
	 * using the provided character set.
	 * 
	 * @param numberOfBytes
	 * @param charset
	 * @return
	 * @throws IOException if an I/O error occurs
	 */
	public String readString(int numberOfBytes, Charset charset) throws IOException
	{
		return new String(readBytes(numberOfBytes), charset);
	}
	
	/**
	 * Reads a single (16 bit) char from the input.
	 * Always uses UTF-16BE encoding (for now).
	 *
	 * @param value char to write
	 * @throws IOException
	 * @see <a href="http://en.wikipedia.org/wiki/UTF-16">http://en.wikipedia.org/wiki/UTF-16</a>
	 */
	public char readChar() throws IOException
	{
		//TODO support other character encodings?
		return (new String(readBytes(2), UTF16BE)).charAt(0);
	}
	
	/**
	 * Closes this stream and the underlying InputStream.
	 * 
	 * @throws IOException if an I/O error occurs
	 */
	public void close() throws IOException
	{
		input.close();
		closed = true;
	}
	
	/* (non-Javadoc)
	 * @see java.io.InputStream#markSupported()
	 */
	public boolean markSupported()
	{
		return false;
	}
	
	/* (non-Javadoc)
	 * @see java.io.InputStream#mark(int)
	 */
	public void	mark(int readlimit)
	{
		//does nothing (mark/reset behaviour is not supported)
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#reset()
	 */
	public void reset() throws IOException
	{
		throw new IOException("Mark/reset not supported");
	}
	
	/* (non-Javadoc)
	 * @see java.io.InputStream#skip(long)
	 */
	public long skip(long n) throws IOException
	{
		if(n > 0)
		{
			long i = 0;
			for(; i < n; i++)
			{
				try
				{
					readByte(true);
				}
				catch(EOFException eof)
				{
					break;
				}
			}
			return i;
		}
		return 0;
	}
	
}
