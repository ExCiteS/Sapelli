package uk.ac.ucl.excites.storage.model;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.text.ParseException;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;

/**
 * @author mstevens
 *
 * @param <T>
 */
public abstract class Column<T>
{
	
	private Class<T> type;
	private Schema schema; // the schema this column belongs to
	private String name;
	protected boolean optional;
	
	public Column(Class<T> type, String name, boolean optional)
	{
		this.type = type;
		this.name = name;
		this.optional = optional;
	}
	
	protected void setSchema(Schema schema)
	{
		if(this.schema != null)
			throw new IllegalStateException("Cannot reassociate column to another schema");
		this.schema = schema;
	}
	
	public void parseAndStoreValue(Record record, String value) throws Exception
	{
		T parsedValue;
		if(value == null || value.equals("")) //empty String is treated as null
			parsedValue = null;
		else
			parsedValue = parse(value);
		storeValue(record, parsedValue);
	}
	
	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 */
	protected abstract T parse(String value) throws ParseException, IllegalArgumentException;
	
	/**
	 * @param record
	 * @param value (as object
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	@SuppressWarnings("unchecked")
	public void storeObject(Record record, Object value) throws IllegalArgumentException, NullPointerException
	{
		storeValue(record, (T) value); 
	}
	
	/**
	 * @param record
	 * @param value
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	public void storeValue(Record record, T value) throws IllegalArgumentException, NullPointerException
	{
		if(!record.getSchema().equals(this.schema))
			throw new IllegalArgumentException("Schema mismatch.");
		if(value == null)
		{
			if(!optional)
				throw new NullPointerException("Cannot set null value for non-optional column!");
			//else: don't store anything (value is null but column is optional)
		}
		else
		{
			validate(value); //throws exception if invalid
			record.setValue(this, value);
		}
	}
	
	public String retrieveValueAsString(Record record)
	{
		T value = retrieveValue(record);
		if(value != null)
			return toString(value);
		else
			return null;
	}
	
	protected abstract String toString(T value); 
	
	/**
	 * Retrieves previously stored value for this column at a given record and casts it to the relevant native type (T)
	 * 
	 * @param record
	 * @return stored value
	 */
	@SuppressWarnings("unchecked")
	public T retrieveValue(Record record)
	{
		if(!record.getSchema().equals(this.schema))
			throw new IllegalArgumentException("Schema mismatch.");
		return (T) record.getValue(this);
	}
	
	/**
	 * Checks whether a value for this column is set in the given record
	 * 
	 * @param record
	 * @return whether or not a (non-null) value is set
	 */
	public boolean isValueSet(Record record)
	{
		return retrieveValue(record) != null;
	}
	
	public final void retrieveAndWriteValue(Record record, BitOutputStream bitStream) throws IOException
	{
		writeValue(retrieveValue(record), bitStream);		
	}

	@SuppressWarnings("unchecked")
	public void writeObject(Object value, BitOutputStream bitStream) throws IOException
	{
		writeValue((T) value, bitStream);
	}
	
	public void writeValue(T value, BitOutputStream bitStream) throws IOException
	{
		if(optional)
			bitStream.write(value != null); //write "presence"-bit
		else
		{
			if(value == null)
				throw new IOException("Non-optional value is null!");
		}
		if(value != null)
		{
			validate(value); //just in case...
			write(value, bitStream); //handled by subclass
		}
	}
	
	protected abstract void write(T value, BitOutputStream bitStream) throws IOException;
	
	public final void readAndStoreValue(Record record, BitInputStream bitStream) throws Exception
	{
		storeValue(record, readValue(bitStream));
	}
	
	public final T readValue(BitInputStream bitStream) throws IOException
	{
		T value = null;
		if(!optional || bitStream.readBit()) //in case of optional column: only read value if "presence"-bit is true
		{
			value = read(bitStream);
			if(value == null)
				throw new IOException(optional ? "Read null value even though presence-bit was set to true!" : "Non-optional value is null!");
			validate(value); //throws exception if invalid
		}
		return value;
	}
	
	/**
	 * @param bitStream
	 * @return
	 * @throws IOException
	 */
	protected abstract T read(BitInputStream bitStream) throws IOException;
	
	/**
	 * Perform checks on potential value (e.g.: not too big for size restrictions, no invalid content, etc.) 
	 * Argument is assumed to be non-null! Hence, null checks should happen before any call of this method.
	 * Throws an IllegalArgumentException in case of invalid value
	 * 
	 * @param value
	 */
	protected abstract void validate(T value) throws IllegalArgumentException;
	
	public T getValueAsStoredBinary(T value)
	{
		BitOutputStream out = null;
		BitInputStream in = null;
		try
		{
			//Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
	
			//Write value:
			writeValue(value, out);
			
			// Flush, close & get bytes:
			out.flush();
			out.close();
	
			//Input stream:
			in = new BitInputStream(new ByteArrayInputStream(rawOut.toByteArray()));
			
			//Read value:
			return readValue(in);
		}
		catch(Exception e)
		{
			System.err.println("Error in retrieveValueAsStoredBinary(Record): " + e.getLocalizedMessage());
			e.printStackTrace();
			return null;
		}
		finally
		{
			try
			{
				if(out != null)
					out.close();
				if(in != null)
					in.close();
			}
			catch(Exception ignore) {}
		}
	}
	
	public T retrieveValueAsStoredBinary(Record record)
	{
		return getValueAsStoredBinary(retrieveValue(record));
	}
	
	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * @return the optional
	 */
	public boolean isOptional()
	{
		return optional;
	}

	public String toString()
	{
		return	getTypeString() + "Column [" +
				name + "; "
				+ (optional ? "optional" : "required") + "; "
				+ getMinimumSize() + (isVariableSize() ? "-" + getMaximumSize() : "") + " bits]";
	}
	
	protected String getTypeString()
	{
		//Note: the null check is necessary for compatibility with data in old DB4O databases
		return (type != null ? type.getSimpleName() : "");
	}
	
	public T retrieveValueCopy(Record record)
	{
		T value = retrieveValue(record);
		return (value == null ? null : copy(value));
	}
	
	/**
	 * Returns a (deep, depending on necessity) copy of the value
	 * 
	 * @param value - assumed to be non-null!
	 * @return
	 */
	protected abstract T copy(T value);
	
	/**
	 * @return whether or not the size taken up by binary stored values for this column varies at run-time (i.e. depending on input)
	 */
	public boolean isVariableSize()
	{
		return optional ? true : (_getMinimumSize() != _getMaximumSize());
	}
	
	/**
	 * Returns the number of bits values for this column take up when written to a binary representation.
	 * In case of a variable size the maximum size is returned.
	 * 
	 * @return
	 */
	public int getSize()
	{
		return getMaximumSize();
	}
	
	/**
	 * Returns the maximum effective number of bits values for this column take up when written to a binary representation, including the presence-bit in case of an optional column.
	 * 
	 * @return
	 */
	public int getMaximumSize()
	{
		return (optional ? 1 : 0) + _getMaximumSize();
	}
	
	/**
	 * Returns the maximum number of bits values for this column take up when written to a binary representation, _without_ the presence-bit in case of an optional column.
	 * 
	 * @return
	 */
	protected abstract int _getMaximumSize();
	
	/**
	 * Returns the minimum effective number of bits values for this column take up when written to a binary representation, including the presence-bit in case of an optional column.
	 * 
	 * @return
	 */
	public int getMinimumSize()
	{
		if(optional)
			return 1;
		else
			return _getMinimumSize();
	}
	
	/**
	 * Returns the minimum number of bits values for this column take up when written to a binary representation, _without_ the presence-bit in case of an optional column.
	 * 
	 * @return
	 */
	protected abstract int _getMinimumSize();
	
	/**
	 * Equality check, compares optionalness, type and size/content restrictions but not the column name
	 * 
	 * @param obj object to compare this one with
	 * @return whether or not the given Object is an identical Column (except for its name)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		return equals(obj, false); // do not check name by default
	}
	
	/**
	 * Equality check, compares optionalness, type and size/content restrictions, AND if checkName is true also the column name
	 * 
	 * @param obj object to compare this one with
	 * @param checkName whether or not to compare the column name
	 * @return whether or not the given Object is an identical/equivalent Column
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj, boolean checkName)
	{
		if(this.getClass().isInstance(obj))
		{
			Column<T> other = (Column<T>) obj;
			if(this.optional != other.optional)
				return false;
			// Check names:
			if(checkName && !this.name.equals(other.name))
				return false;
			// Check restrictions (size/content):
			return equalRestrictions(other);
		}
		else
			return false;
	}
	
	protected abstract boolean equalRestrictions(Column<T> otherColumn);

	/**
	 * Can be overridden by subclasses
	 * 
	 * @param separator
	 * @return
	 */
	public String getCSVHeaderLabel(String separator)
	{
		return name;
	}
	
}