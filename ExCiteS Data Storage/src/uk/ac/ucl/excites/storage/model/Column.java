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
	
	protected abstract T parse(String value);
	
	public void storeValue(Record record, T value) throws Exception
	{
		if(this.schema != record.getSchema())
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
		T value = (T) schema.getTable().get(record, this);
		if(!optional && value == null)
			throw new IllegalStateException("Could not a find a value for this record & column, even though the column is non-optional!");
		return value;
	}
	
	public final void retrieveAndWriteValue(Record record, BitOutputStream bitStream) throws IOException
	{
		writeValue(retrieveValue(record), bitStream);		
	}
	
	public final void writeValue(T value, BitOutputStream bitStream) throws IOException
	{
		validate(value); //just in case...
		if(optional)
			bitStream.write(value != null); //write "presence"-bit
		else
		{
			if(value == null)
				throw new IllegalStateException("Non-optional value is null!");
		}
		if(value != null)
			write(value, bitStream); //handled by subclass
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
			validate(value); //throw exception if invalid
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
