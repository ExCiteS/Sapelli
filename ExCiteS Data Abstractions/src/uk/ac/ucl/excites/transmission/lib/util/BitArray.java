package uk.ac.ucl.excites.transmission.lib.util;

import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.Iterator;


/**
 * @author mstevens
 *
 */
public class BitArray 
{

	private static final Charset DEFAULT_CHARSET = Charset.forName("UTF-8");

	private boolean[] bits;
	
	
	public BitArray(int size)
	{
		bits = new boolean[size];
	}
	
	public int size()
	{
		return bits.length;
	}
	
	public boolean get(int index)
	{
		if(index < 0 || index >= bits.length)
			throw new IndexOutOfBoundsException("Index (" + index + ") out of bounds!");
		return bits[index];
	}
	
	public int set(int index, boolean bit)
	{
		if(index < 0 || index >= bits.length)
			throw new IndexOutOfBoundsException("Index (" + index + ") out of bounds!");
		bits[index] = bit;
		return index++;
	}
	
	public boolean[] getBits(int index, int length)
	{
		if(index < 0 || index > bits.length - length)
			throw new IndexOutOfBoundsException("Index (" + index + ") out of bounds!");
		boolean[] subarray = new boolean[length];
		for(int i = 0; i < length; i++)
			subarray[i] = bits[index+i];
		return subarray;
	}
	
	public int setBits(int index, boolean[] bits)
	{
		if(index < 0 || index > this.bits.length - bits.length)
			throw new IndexOutOfBoundsException("Index (" + index + ") out of bounds!");
		for(boolean bit : bits)
			this.bits[index++] = bit;
		return index + bits.length;
	}
	
	public byte getByte(int index)
	{
		return getByte(index, true); //default behaviour is strict
	}
	
	/**
	 * @param index
	 * @param strict 	if true exactly 8 bits must be read from the array;
	 * 					if false at least one bit needs to be read, the other -most significant- bits will be set to 0 if going out of bounds at the end of the array
	 * @return
	 */
	public byte getByte(int index, boolean strict)
	{
		if(index < 0 || index > bits.length - (strict ? 8 : 1))
			throw new IndexOutOfBoundsException("Index (" + index + ") out of bounds!");
		byte b = 0;
		for(int i = 0; i < 8; i++)
	        if(index+i < bits.length && bits[index+i])
	            b |= 1 << i;
		return b;
	}
	
	public void setByte(int index, byte b)
	{
		if(index < 0 || index > bits.length - 8)
			throw new IndexOutOfBoundsException("Index (" + index + ") out of bounds!");
		for(int i = 0; i < 8; i++)
			bits[index+i] = ((b & (1 << i)) != 0);
	}

	public byte[] getBytes(int index, int numberOfBytes)
	{
		return getBytes(index, numberOfBytes, true); //default behaviour is strict		
	}
	
	/**
	 * @param index
	 * @param numberOfBytes
	 * @param strict 	if true all bytes must be read entirely from the array;
	 * 					if false up to 7 bits of the last byte can be out of bounds and will be set to 0
	 * @return
	 */
	public byte[] getBytes(int index, int numberOfBytes, boolean strict)
	{
		int indexOfLastByte = index + (numberOfBytes - 1) *8;
		System.out.println(indexOfLastByte);
		if(index < 0 || indexOfLastByte > bits.length - (strict ? 8 : 1))
			throw new IndexOutOfBoundsException("Index (" + (strict ? bits.length : indexOfLastByte) + ") out of bounds!");
		byte[] result = new byte[numberOfBytes];
		for(int i = 0; i < numberOfBytes; i++)
			result[i] = getByte(index + (i * 8), strict);
		return result;
	}
	
	public void setBytes(int index, byte[] bytes)
	{
		if(index < 0 || index > bits.length - (bytes.length * 8))
			throw new IndexOutOfBoundsException("Index (" + index + ") out of bounds!");
		for(byte b : bytes)
		{
			setByte(index, b);
			index += 8;
		}
	}
	
	public long getInteger(int index, int numberOfBits, boolean signed/*, ByteOrder order*/)
	{
		//LITTLE ENDIAN BYTE ORDER
		//TODO Big endian byte order
		
		if(numberOfBits > 64)
			throw new IllegalArgumentException("Cannot store more than 64 bits in value of type long");
		long value = 0;
		for (int i = 0; i < numberOfBits; i++)
			value += get(index + i) ? (1l << i) : 0l;
		if(signed && value >= (long) Math.pow(2, numberOfBits - 1))
			value -= (long) Math.pow(2, numberOfBits);
		return value;
	}
	
	public void setInteger(int index, long value, int numberOfBits, boolean signed/*, ByteOrder order*/)
	{
		//LITTLE ENDIAN BYTE ORDER
		//TODO Big endian byte order
		
		if(signed)
		{	//Signed
			if(value < (long) (- Math.pow(2, numberOfBits - 1)) || value > (long) (Math.pow(2, numberOfBits - 1) - 1))
				throw new IllegalArgumentException("Signed value (" + value + ") does not fit in " + numberOfBits + " bits.");
		}
		else
		{	//Unsigned
			if(!signed && value < 0)
				throw new IllegalArgumentException("Cannot write negative value as unsigned interger.");
			if(value > (long) Math.pow(2, numberOfBits) - 1)
				throw new IllegalArgumentException("Unsigned value (" + value + ") does not fit in " + numberOfBits + " bits.");
		}
		
		if(signed && value < 0)
			value += Math.pow(2, numberOfBits); //Two's complement
		int i = 0;
		while(value != 0l && i < numberOfBits)
		{
			set(index + i, value % 2l != 0);
			i++;
			value = value >>> 1;
		}
		//padding:
		while(i < numberOfBits)
		{
			set(index + i, false);
			i++;
		}
	}
	
//	public float getFloat(int index)
//	{
//		return 0f;
//	}
//	
//	public void setFloat(int index, float f)
//	{
//		//setInteger(start, 4, true, Float.floatToRawIntBits(f), ByteOrder.nativeOrder());
//	}
//	
//	public double getDouble(int index)
//	{
//		return 0d;
//	}
//	
//	public void setDouble(int index, double d)
//	{
//		
//	}
	
	public String getString(int index, int numberOfBytes)
	{
		return new String(getBytes(index, numberOfBytes), DEFAULT_CHARSET);
	}
	
	public String getString(int index, int numberOfBytes, Charset charset)
	{
		return new String(getBytes(index, numberOfBytes), charset);
	}
	
	public int setString(int index, String string)
	{
		byte[] bytes = string.getBytes(DEFAULT_CHARSET);
		setBytes(index, bytes);
		return bytes.length;
	}
	
	public int setString(int index, String string, Charset charset)
	{
		byte[] bytes = string.getBytes(charset);
		setBytes(index, bytes);
		return bytes.length;
	}

	public Iterator<Boolean> bits()
	{
		return new Iterator<Boolean>()
		{
			private int i = 0;
			
			public boolean hasNext()
			{
				return i < bits.length;
			}

			public Boolean next()
			{
				return bits[i++];
			}

			public void remove()
			{
				throw new UnsupportedOperationException("Cannot remove elements");
			}
		};
	}
	
	public Iterator<Byte> bytes()
	{
		return bytes(true); //default behaviour is strict
	}

	/**
	 * @param strict 	if true the last bits will not be read if the array size is not a multiple of 8;
	 * 					if false the last bits will be read and returned as a byte with the "non-existing" higher bits set to 0
	 * @return
	 */
	public Iterator<Byte> bytes(final boolean strict)
	{
		
		return new Iterator<Byte>()
		{
			private int i = 0;
			
			public boolean hasNext()
			{
				return i <= bits.length - (strict ? 8 : 1);
			}

			public Byte next()
			{
				byte b = getByte(i, strict);
				i += 8;
				return b;
			}

			public void remove()
			{
				throw new UnsupportedOperationException("Cannot remove elements");
			}
		};
	}
	
	public Iterator<Long> integers(int sizeBits, boolean signed, ByteOrder bo)
	{
		return null;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args)
	{
		//String lipsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";
		//byte[] uncompressed = lipsum.getBytes(Charset.forName("UTF-8"));
//		BitArray ba = new BitArray(8*lipsum.length()+7);
//		ba.setBytes(0, lipsum.getBytes(Charset.forName("UTF-8")));
//		
//		Iterator<Byte> bi = ba.bytes();
//		while(bi.hasNext())
//			System.out.println(toBitString(bi.next()));
//		
//		bi = ba.bytes();
//		while(bi.hasNext())
//			System.out.print((char) bi.next().byteValue());
//		
//		System.out.println("\n");
//		System.out.println(Long.toBinaryString(13));
		
//		int index = 0;
//		int number = 64;
//		byte numberAsByte = (new Integer(number)).byteValue();
//		
//		
//		BitArray ba = new BitArray(23);
//		ba.setByte(index, numberAsByte);
//		for(int i = 0; i < ba.size(); i++)
//			System.out.print(ba.get(i) ? "1" : "0");
//		System.out.println("");
//		
//		System.out.println(numberAsByte == ba.getByte(index));
//		
//		byte[] byteArray = { numberAsByte };
//		BitSet bs = BitSet.valueOf(byteArray);
//		for(int i = 0; i < bs.size(); i++)
//			System.out.print(bs.get(i) ? "1" : "0");
//		System.out.println("");
//		
//		System.out.println(toBinaryString(ba.getByte(index)));
//		
//		System.out.println(Integer.toBinaryString(number));
//		
//		boolean[] fivebits = { true, false, false, true, true };
//		ba.setBits(18, fivebits);
//		for(int i = 0; i < ba.size(); i++)
//			System.out.print(ba.get(i) ? "1" : "0");
//		System.out.println("");
		
//		BitArray ba = new BitArray(128);
//
//		
//		
//		for(long i = Integer.MIN_VALUE; i <= Integer.MAX_VALUE; i+=100)
//		{
//			int index = 0;
//		
//			//System.out.println(i);
////			String bitString = Long.toBinaryString(i);
////			StringBuffer bff = new StringBuffer(bitString);
////			bff.reverse();
////			String bitString2 = bff.toString();
//			//System.out.println(bitString2 + " (length: " + bitString.length() + ")");
//		
//			
//			ba.setInteger(index, i, 32, true);
//
//			
//			//for(int j = 0; j < 32; j++)
//			//	System.out.print(ba.get(j) ? "1" : "0");
//			//System.out.println("");
//			
//			long readNumber = ba.getInteger(0, 32, true);
//			
//			if(i != readNumber)
//				System.out.println("Error " + i + " " + readNumber);
//			
//			
//			//for(int i = 0; i < ba.size(); i++)
//			//System.out.print(ba.get(i) ? "1" : "0");
//		}
//		System.out.println("Done");
		
//		BitArray ba2 = new BitArray(128);
//		ba2.setInteger(0, 1000, 16, true);
//		
//		System.out.println(BinaryHelpers.bin2Hex(ba2.getBytes(0, 1)));

		int i = 8;
		i <<= 1;
		i++;
		System.out.println(i);
		
		
	}


//	public byte[] toBytes()
//	{
//		
//		return null;
//	}
	

	
}
