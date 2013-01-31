package uk.ac.ucl.excites.transmission.lib.model;
/**
 * 
 */

import java.io.IOException;
import java.nio.charset.Charset;

import uk.ac.ucl.excites.transmission.lib.io.BitInputStream;
import uk.ac.ucl.excites.transmission.lib.io.BitOutputStream;
import uk.ac.ucl.excites.transmission.lib.util.IntegerRangeMapping;
import uk.ac.ucl.excites.transmission.lib.util.StringUtils;

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
	
	public StringColumn(String name)
	{
		this(name, DEFAULT_MAX_LENGTH, DEFAULT_CHARSET);
	}
	
	public StringColumn(String name, int maxLengthBytes)
	{
		this(name, maxLengthBytes, DEFAULT_CHARSET);
	}
	
	public StringColumn(String name, int maxLengthBytes, Charset charset)
	{
		super(name);
		this.maxLengthBytes = maxLengthBytes;
		this.charset = charset;
	}

	@Override
	protected String parse(String value)
	{
		//this is already a string
		return value;
	}

	@Override
	protected void validate(String value) throws IllegalArgumentException
	{
		int bytesNeeded = StringUtils.sizeBytes(value, charset);
		if(bytesNeeded > maxLengthBytes)
			throw new IllegalArgumentException("String is too long (it would take " + bytesNeeded + " bytes, while the maximum allowed is " + maxLengthBytes + " bytes).");
	}

	@Override
	protected void write(String value, BitOutputStream bitStream) throws IOException
	{
		//Write length:
		//TODO write lenth
		//int bitsNeededForMaxSize = ;
		//bitStream.write(StringUtils.sizeBytes(value, charset), bitsNeededForMaxSize, false);
		//Write actual string:
		bitStream.write(value, charset);
	}

	@Override
	protected String read(BitInputStream bitStream) throws IOException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isVariableSize()
	{
		return true;
	}

	/**
	 * Returns the maximum, effective (including the length field) number of bits this column will take up
	 * 
	 * @see uk.ac.ucl.excites.transmission.lib.model.Column#getSize()
	 */
	@Override
	public int getSize()
	{
		int bitsNeededForLengthField = 0;
		//TODO compute bits used for size field!!
		return bitsNeededForLengthField + (maxLengthBytes * 8);
	}
	
}
