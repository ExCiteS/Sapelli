package uk.ac.ucl.excites.storage.model;


import java.io.IOException;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;

public abstract class Column<T>
{
	
	private Schema schema; // the schema this column belongs to
	private String name;
	protected boolean optional;
	
	protected Column(String name, boolean optional)
	{
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
	 */
	protected abstract T parse(String value);
	
	public void storeValue(Record record, T value) throws IOException
	{
		if(this.schema != record.getSchema())
			throw new IllegalArgumentException("Schema mismatch.");
		if(value == null)
		{
			if(!optional)
				throw new IOException("Cannot set null value for non-optional column!");
			//else: don't store anything (value is null but column is optional)
		}
		else
		{
			validate(value); //throws exception if invalid
			schema.getTable().put(record, this, value);
		}
	}
	
	/**
	 * Retrieves previously stored value for this column at a given record and casts it to the relevant native type (T)
	 * 
	 * @param record
	 * @return stored value
	 */
	@SuppressWarnings("unchecked")
	public T retrieveValue(Record record)
	{
		if(this.schema != record.getSchema())
			throw new IllegalArgumentException("Schema mismatch.");
		return (T) schema.getTable().get(record, this);
	}
	
	public final void retrieveAndWriteValue(Record record, BitOutputStream bitStream) throws IOException
	{
		writeValue(retrieveValue(record), bitStream);		
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
	
	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}

	public String toString()
	{
		return "Column [" + name + "]";
	}
	
	public abstract boolean isVariableSize();
	
	/**
	 * Returns the number of bits values for this column take up when writen to a binary representation
	 * In case of a variable size the maximum effective size is returned
	 * 
	 * @return
	 */
	public abstract int getSize();
	
}
