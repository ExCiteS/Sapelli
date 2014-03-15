package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * A column for 32 bit (float) or 64 bit (double) floating point numbers
 * 
 * @author mstevens
 */
public class FloatColumn extends Column<Double>
{	
	
	static public final boolean DEFAULT_DOUBLE_PRECISION = false; // 32 bit (float) by default
	
	private boolean doublePrecision;
	
	public FloatColumn(String name, boolean optional)
	{
		this(name, optional, DEFAULT_DOUBLE_PRECISION);
	}
	
	public FloatColumn(String name, boolean optional, boolean doublePrecision)
	{
		super(Double.class, name, optional);
		this.doublePrecision = doublePrecision;
	}

	@Override
	public FloatColumn copy()
	{
		return new FloatColumn(name, optional, doublePrecision);
	}
	
	/**
	 * Float version of {@link FloatColumn#storeValue(Record, Double)}
	 * 
	 * @param record
	 * @param value
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	public void storeValue(Record record, Float value) throws IllegalArgumentException, NullPointerException
	{
		Double doubleValue = (value != null ? Double.valueOf(value.floatValue()) : null);
		storeValue(record, doubleValue);
	}
	
	/**
	 * @param record
	 * @param nullReplacement
	 * @return
	 */
	public double getPrimitiveDouble(Record record, double nullReplacement)
	{
		Double doubleValue = retrieveValue(record);
		if(doubleValue == null)
			return nullReplacement;
		return doubleValue.doubleValue();
	}
	
	/**
	 * @param record
	 * @param nullReplacement
	 * @return
	 */
	public float getPrimitiveFloat(Record record, float nullReplacement)
	{
		Double doubleValue = retrieveValue(record);
		if(doubleValue == null)
			return nullReplacement;
		return doubleValue.floatValue();
	}

	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 * @throws NumberFormatException
	 */
	@Override
	protected Double parse(String value) throws NumberFormatException
	{
		return Double.valueOf(value);
	}

	@Override
	protected void validate(Double value) throws IllegalArgumentException
	{
		/* Does nothing
		 * Note: I originally planned to check whether the value could
		 * 		 fit in a 32 bit float when in single precision mode,
		 * 		 but there is no obvious (or even correct) way to do this(?). 
		 */
	}

	@Override
	protected void write(Double value, BitOutputStream bitStream) throws IOException
	{
		if(doublePrecision)
			bitStream.write(value);
		else
			bitStream.write(value.floatValue());
	}

	@Override
	protected Double read(BitInputStream bitStream) throws IOException
	{
		return doublePrecision ? bitStream.readDouble() : bitStream.readFloat();
	}

	@Override
	protected int _getMinimumSize()
	{
		return doublePrecision ? Double.SIZE : Float.SIZE;
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return doublePrecision ? Double.SIZE : Float.SIZE;
	}

	@Override
	protected String toString(Double value)
	{
		return value.toString();
	}

	@Override
	protected boolean equalRestrictions(Column<Double> otherColumn)
	{
		if(otherColumn instanceof FloatColumn)
			return this.doublePrecision == ((FloatColumn) otherColumn).doublePrecision;
		return false;
	}

	@Override
	protected Double copy(Double value)
	{
		return Double.valueOf(value);
	}
	
	/**
	 * Even though the type is actually Double we have called this column an "FloatColumn" 
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#getTypeString()
	 */
	@Override
	public String getTypeString()
	{
		return Float.class.getSimpleName();
	}
	
}
