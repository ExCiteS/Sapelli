package uk.ac.ucl.excites.storage.model;
/**
 * 
 */

import java.io.IOException;
import java.nio.charset.Charset;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.util.StringUtils;

/**
 * A column for Strings
 * 
 * The empty String ("") is treated as null and is therefore only allowed if the column is optional
 * 
 * @author mstevens
 */
public class StringColumn extends Column<String>
{
	
	//STATIC
	private static final Charset DEFAULT_CHARSET = Charset.forName("UTF-8");
	private static final int DEFAULT_MAX_LENGTH = 256; //bytes
	
	//DYNAMIC
	private int maxLengthBytes;
	private Charset charset;
	
	private IntegerRangeMapping sizeField;
	
	public StringColumn(String name, boolean optional)
	{
		this(name, optional, DEFAULT_MAX_LENGTH, DEFAULT_CHARSET);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes) a String stored in this column can be
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes)
	{
		this(name, optional, maxLengthBytes, DEFAULT_CHARSET);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes) a String stored in this column can be
	 * @param charset the charset to use to encode/decode Strings to/from bytes
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, Charset charset)
	{
		super(String.class, name, optional);
		this.maxLengthBytes = maxLengthBytes; //TODO check on value
		this.charset = charset;
		this.sizeField = new IntegerRangeMapping(1, maxLengthBytes); //we don't store the empty string so effective size is always at least 1 byte
	}
	
	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 */
	@Override
	protected String parse(String value)
	{
		//this is already a string
		return value;
	}

	/**
	 * Checks for empty string (only allowed if column is optional) and size restriction violations.
	 * 
	 * @see uk.ac.ucl.excites.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(String value) throws IllegalArgumentException
	{
		if(value.equals(""))
		{
			if(!optional)
				throw new IllegalArgumentException("Empty String (which we treat the same as a null value) is not allowed in non-optional column");
		}
		else
		{
			int bytesNeeded = StringUtils.sizeBytes(value, charset);
			if(bytesNeeded > maxLengthBytes)
				throw new IllegalArgumentException("String \"" + value + "\" is too long (it would take " + bytesNeeded + " bytes, while the maximum allowed is " + maxLengthBytes + " bytes).");
		}
	}
	
	/**
	 * StringColumn overrides Column#writeValue(T, BitOutputStream) to force that the empty String ("") is treated as null
	 * 
	 * @see uk.ac.ucl.excites.storage.model.Column#writeValue(java.lang.Object, uk.ac.ucl.excites.storage.io.BitOutputStream)
	 */
	@Override
	public void writeValue(String value, BitOutputStream bitStream) throws IOException
	{
		super.writeValue("".equals(value) ? null : value, bitStream);
	}

	@Override
	protected void write(String value, BitOutputStream bitStream) throws IOException
	{
		//Write length:
		sizeField.write(StringUtils.sizeBytes(value, charset), bitStream);
		//Write actual string:
		bitStream.write(value, charset);
	}

	@Override
	protected String read(BitInputStream bitStream) throws IOException
	{
		//Read length:
		int numberOfBytes = (int) sizeField.read(bitStream);
		//Read actual string:
		return bitStream.readString(numberOfBytes, charset);
	}

	@Override
	protected int _getMinimumSize()
	{
		return sizeField.getSize() + Byte.SIZE; //TODO isn't it at least 2, if we do a proper check on maxLengthBytes in constructor it must be at least 2, unless we start accepting empty strings
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return sizeField.getSize() + maxLengthBytes * Byte.SIZE;
	}

	@Override
	protected String toString(String value)
	{
		return value;
	}
	
	@Override
	protected boolean equalRestrictions(Column<String> otherColumn)
	{
		if(otherColumn instanceof StringColumn)
		{
			StringColumn other = (StringColumn) otherColumn;
			return this.maxLengthBytes == other.maxLengthBytes && this.charset.equals(other.charset);
		}
		else
			return false;
	}

	@Override
	protected String copy(String value)
	{
		return new String(value);
	}
	
}
