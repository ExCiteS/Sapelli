/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;

/**
 * A class representing records of a certain Schema
 * 
 * @author mstevens
 */
public class Record implements Serializable
{
	
	// Statics-------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static public final String TAG_RECORD = "Record";
	static final private char SERIALISATION_SEPARATOR = ';';
	static final private char SERIALISATION_SEPARATOR_ESCAPE = ':';
	static final private char SERIALISATION_SEPARATOR_ESCAPE_PREFIX = '/';
	
	// Dynamics------------------------------------------------------
	protected Schema schema;
	protected Object[] values;
	
	protected String exportComment;
	protected boolean exported = false;
	
	protected boolean sent = false;
	protected DateTime sendingAttemptedAt = null;
	
	public Record(Schema schema)
	{
		if(schema == null)
			throw new NullPointerException("Schema cannot be null!");
		if(!schema.isSealed())
			throw new IllegalStateException("Schema must be sealed before records based on it can be created!");
		this.schema = schema;
		values = new Object[schema.getNumberOfColumns(false)];
	}
	
	public Record(Schema schema, Object... values)
	{
		this(schema);
		if(values != null)
		{	
			if(this.values.length == values.length)
				// Init from given values:
				for(int i = 0; i < values.length; i++)
					this.values[i] = values[i];
			else
				throw new IllegalArgumentException("Unexpected number of values (given: " + values.length + "; expected: " + this.values.length + ").");
		}
	}
	
	/**
	 * Copy contructor
	 * 
	 * @param another
	 */
	public Record(Record another)
	{
		this(another.schema);
		
		//(Deep) copy of values:
		for(Column<?> c : this.schema.getColumns(false))
			setValue(c, c.retrieveValueCopy(another));
	}
	
	/**
	 * @return the schema
	 */
	public Schema getSchema()
	{
		return schema;
	}
	
	/**
	 * Override the schema object with another one, if compatible
	 * 
	 * @param newSchema
	 * @throws IllegalArgumentException - when the new schema is incompatible with the old one
	 */
	public void setSchema(Schema newSchema) throws IllegalArgumentException
	{
		setSchema(newSchema, false);
	}
	
	/**
	 * Override the schema object with another one, if compatible (unless forced)
	 * 
	 * @param newSchema
	 * @param force - if true the old and new schema are *not* compared, but the number of columns of the new schema must *always* match the number of values!
	 * @throws IndexOutOfBoundsException - when the new schema has a different number of columns than the number of values in the record
	 * @throws IllegalArgumentException - when the new schema is incompatible with the old one
	 */
	public void setSchema(Schema newSchema, boolean force) throws IndexOutOfBoundsException, IllegalArgumentException
	{
		if(force)
		{		
			if(newSchema.getNumberOfColumns(false) != values.length)
				throw new IndexOutOfBoundsException("The new schema has a different number of columns than the number of values in this record!");
		}
		else
		{
			if(!schema.equals(newSchema, true, true, false)) // also checks columns, but not indexes
				throw new IllegalArgumentException("The provived schema is not compatible with this record!");
		}
		this.schema = newSchema; // we accept the new one
	}
	
	/**
	 * To be called from {@link Column#storeValue(Record, Object)}
	 * 
	 * @param column
	 * @param value
	 */
	protected void setValue(Column<?> column, Object value)
	{
		// Deal with virtual columns:
		if(column instanceof VirtualColumn)
			throw new IllegalArgumentException("Records do not hold values of virtual columns!"); // this should never happen
		// Get column position:
		int position = schema.getColumnPosition(column);
		// Check position:
		if(position == Schema.UNKNOWN_COLUMN_POSITION)
			throw new IllegalArgumentException("The schema of this record has no such column (\"" + column.name + "\").");
		// Get value:
		values[position] = value;
	}
	
	/**
	 * To be called from {@link Column#retrieveValue(Record)}
	 * 
	 * @param column
	 */
	protected Object getValue(Column<?> column)
	{
		// Deal with virtual columns:
		if(column instanceof VirtualColumn)
			throw new IllegalArgumentException("Records do not hold values of virtual columns!"); // this should never happen
		// Get column position:
		int position = schema.getColumnPosition(column);
		// Check position:
		if(position == Schema.UNKNOWN_COLUMN_POSITION)
			throw new IllegalArgumentException("The schema of this record has no such column (\"" + column.name + "\").");
		// Set value:
		return values[position];
	}

	/**
	 * Checks whether all non-optional columns have been assigned a (non-null) value
	 * 
	 * @return
	 */
	public boolean isFilled()
	{
		for(Column<?> c : schema.getColumns(false))
			if(getValue(c) == null && !c.isOptional())
				return false; // null value in non-optional column	
		return true;
	}
	
	public void writeToBitStream(BitOutputStream bitStream, boolean includeVirtual, Set<Column<?>> skipColumns) throws IOException
	{
		try
		{	//write fields:
			for(Column<?> c : schema.getColumns(includeVirtual))
				if(!skipColumns.contains(c))
					c.retrieveAndWriteValue(this, bitStream);
		}
		catch(Exception e)
		{
			throw new IOException("Error on attempting to write record", e);
		}
	}
	
	public void readFromBitStream(BitInputStream bitStream, boolean includeVirtual, Set<Column<?>> skipColumns) throws IOException
	{
		try
		{	//read fields:
			for(Column<?> c : schema.getColumns(includeVirtual))
				if(!skipColumns.contains(c))
					if(c instanceof VirtualColumn)
						c.readValue(bitStream); // read but don't store values of virtual columns (i.e. we skip them in the stream)
					else
						c.readAndStoreValue(this, bitStream);
		}
		catch(Exception e)
		{
			throw new IOException("Error on attempting to read record. Read so far: " + this.toString(), e);
		}
	}
	
	/**
	 * Gets the size of this record in number of bits
	 * 
	 * @return
	 */
	public int getSize(boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		BitOutputStream out = null;
		try
		{
			out = new BitOutputStream(new ByteArrayOutputStream());
			this.writeToBitStream(out, includeVirtual, skipColumns);
			return out.getNumberOfBitsWritten();
		}
		catch(IOException e)
		{
			System.err.println("Error upon calculating record size: " + e.getLocalizedMessage());
			e.printStackTrace(System.err);
			return -1;
		}
		finally
		{
			if(out != null)
				try
				{
					out.close();
				}
				catch(IOException ignore) {}
		}
	}
	
	/**
	 * @return the exported
	 */
	public boolean isExported()
	{
		return exported;
	}

	/**
	 * @param exported the exported to set
	 */
	public void setExported(boolean exported)
	{
		this.exported = exported;
	}

	/**
	 * @return the sent
	 */
	public boolean isSent()
	{
		return sent;
	}

	/**
	 * @param sent the sent to set
	 */
	public void setSent(boolean sent)
	{
		this.sent = sent;
	}

	/**
	 * @return the sendingAttemptedAt - if sent=true this is the timestamp of the last (successful) attempt 
	 */
	public DateTime getSendingAttemptedAt()
	{
		return sendingAttemptedAt;
	}

	/**
	 * @param sendingAttemptedAt the sendingAttemptedAt to set
	 */
	public void setSendingAttemptedAt(DateTime sendingAttemptedAt)
	{
		this.sendingAttemptedAt = sendingAttemptedAt;
	}

	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + schema.hashCode();
		hash = 31 * hash + Arrays.hashCode(values);
		return hash;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		return equals(obj, true);
	}
	
	public boolean equals(Object obj, boolean checkSchema)
	{
		if(obj instanceof Record)
		{
			Record other = (Record) obj;
			if(checkSchema)
			{	// Check if records have the same (or 100% equivalent) schema
				if(this.schema.equals(other.schema))
					return false;
			}
			else
			{	// Only check if the number of columns matches (to avoid out or range errors below):
				if(this.schema.getNumberOfColumns(false) != other.schema.getNumberOfColumns(false))
					return false;
			}
			// Compare values for each column (using values as if decoded from binary stream):
			return hasEqualValues(other, true);
		}
		else
			return false;
	}
	
	public boolean hasEqualValues(Record other, boolean asStoredBinary)
	{
		return hasEqualValues(other, this.schema.getColumns(false), asStoredBinary); // compare all non-virtual columns
	}
	
	/**
	 * Compare the values of this records with those of another, across the given list of columns.
	 * This and the other record as assumed to have schemata that are the same or at least each contain the given columns (or equivalents).
	 * 
	 * @param other
	 * @param columns
	 * @param asStoredBinary
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	public boolean hasEqualValues(Record other, List<Column> columns, boolean asStoredBinary)
	{
		for(Column c : columns)
		{
			if(!EqualValues(asStoredBinary ? c.retrieveValueAsStoredBinary(this) : c.retrieveValue(this),
							asStoredBinary ? c.retrieveValueAsStoredBinary(other) : c.retrieveValue(other)))
				return false;
		}
		return true;
	}
	
	@Override
	public String toString()
	{
		return toString(true);
	}
	
	public String toString(boolean includeVirtual)
	{
		StringBuffer bff = new StringBuffer();
		bff.append(schema.toString());
		for(Column<?> c : schema.getColumns(includeVirtual))
			bff.append("|" + c.getName() + ": " + c.retrieveValueAsString(this));
		return bff.toString();
	}
	
	/**
	 * Serialise a the Record to a String
	 * 
	 * @return
	 */
	public String serialise()
	{
		return serialise(false, Collections.<Column<?>>emptySet());
	}
	
	/**
	 * Serialise a the Record to a String
	 * 
	 * @param skipColumns
	 * @return
	 */
	public String serialise(boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		StringBuilder bldr = new StringBuilder();
		boolean first = true;
		for(Column<?> col : schema.getColumns(includeVirtual))
		{
			// Separator:
			if(first)
				first = false;
			else
				bldr.append(SERIALISATION_SEPARATOR); // also when value is skipped below!
			// Value:
			if(skipColumns == null || !skipColumns.contains(col))
			{
				String valueString = col.retrieveValueAsString(this);
				bldr.append(valueString == null ? "" : StringUtils.escape(valueString, SERIALISATION_SEPARATOR, SERIALISATION_SEPARATOR_ESCAPE, SERIALISATION_SEPARATOR_ESCAPE_PREFIX));
			}
		}
		return bldr.toString();
	}
	
	/**
	 * Deserialise the values of a Record from a String
	 * 
	 * @param serialisedRecord
	 * @throws Exception
	 * @return the Record itself
	 */
	public Record parse(String serialisedRecord) throws Exception
	{
		return parse(serialisedRecord, false, Collections.<Column<?>>emptySet());
	}
	
	/**
	 * Deserialise the values of a Record from a String
	 * 
	 * @param serialisedRecord
	 * @param skipColumns
	 * @throws Exception
	 * @return the Record itself
	 */
	public Record parse(String serialisedRecord, boolean includeVirtual, Set<Column<?>> skipColumns) throws ParseException, IllegalArgumentException, NullPointerException
	{
		String[] parts = serialisedRecord.split("\\" + SERIALISATION_SEPARATOR);
		if(parts.length != schema.getNumberOfColumns(includeVirtual))
			throw new IllegalArgumentException("Unexpected number of values (got: " + parts.length + "; expected: " + schema.getNumberOfColumns(includeVirtual) + ") in serialised record (" + serialisedRecord +  ").");
		int p = 0;
		for(Column<?> col : schema.getColumns(includeVirtual))
		{
			if(!(col instanceof VirtualColumn) && !skipColumns.contains(col)) // skip virtual columns & skipColumns (but *do* increment the counter p!)
				col.parseAndStoreValue(this, StringUtils.deescape(parts[p], SERIALISATION_SEPARATOR, SERIALISATION_SEPARATOR_ESCAPE, SERIALISATION_SEPARATOR_ESCAPE_PREFIX));
			p++;
		}
		return this;
	}
	
	public byte[] toBytes() throws IOException
	{
		BitOutputStream out = null;
		try
		{
			//Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
				
			//Write record:
			this.writeToBitStream(out, false, Collections.<Column<?>> emptySet());
			
			//Flush & close the stream and get bytes:
			out.flush();
			out.close();
			return rawOut.toByteArray();
		}
		catch(Exception e)
		{
			throw new IOException("Error on encoding record.", e);
		}
		finally
		{
			try
			{
				if(out != null)
					out.close();
			}
			catch(Exception ignore) {}
		}
	}
	
	/**
	 * Helper method to compare 2 potentially null objects
	 * 
	 * @param value1
	 * @param value2
	 * @return
	 */
	static public boolean EqualValues(Object value1, Object value2)
	{
		if(value1 != null)
			return value1.equals(value2);
		else
			return value2 == null;
	}
	
}
