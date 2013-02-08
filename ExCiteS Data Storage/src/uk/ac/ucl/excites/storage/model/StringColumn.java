package uk.ac.ucl.excites.storage.model;
/**
 * 
 */

import java.io.IOException;
import java.nio.charset.Charset;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.storage.util.StringUtils;

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
	private static final int DEFAULT_MAX_LENGTH = 256;
	
	//DYNAMIC
	private int maxLengthBytes;
	private IntegerRangeMapping sizeField;
	private Charset charset;
	
	public StringColumn(String name, boolean optional)
	{
		this(name, optional, DEFAULT_MAX_LENGTH, DEFAULT_CHARSET);
	}
	
	public StringColumn(String name, boolean optional, int maxLengthBytes)
	{
		this(name, optional, maxLengthBytes, DEFAULT_CHARSET);
	}
	
	public StringColumn(String name, boolean optional, int maxLengthBytes, Charset charset)
	{
		super(name, optional);
		this.maxLengthBytes = maxLengthBytes;
		this.charset = charset;
		this.sizeField = new IntegerRangeMapping(1, maxLengthBytes); //we don't store the empty string so effective size is always at least 1
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
		super.writeValue(value.equals("") ? null : value, bitStream);
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
	public boolean isVariableSize()
	{
		return true;
	}

	/**
	 * Returns the maximum, effective (including the length field) number of bits this column will take up
	 * 
	 * @see uk.ac.ucl.excites.storage.model.Column#getSize()
	 */
	@Override
	public int getSize()
	{
		return sizeField.getSize() + (maxLengthBytes * 8);
	}
	
}
