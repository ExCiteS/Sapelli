/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;

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
	static final private char SERIALISATION_SEPARATOR = ',';
	static final private char SERIALISATION_SEPARATOR_ESCAPE = '.';
	static final private char SERIALISATION_SEPARATOR_ESCAPE_PREFIX = '/';
	
	// Dynamics------------------------------------------------------
	protected Schema schema;
	protected Object[] values;
	
	/**
	 * Creates a new "empty" record of the given schema
	 * 
	 * @param schema
	 */
	protected Record(Schema schema)
	{
		if(schema == null)
			throw new NullPointerException("Schema cannot be null!");
		if(!schema.isSealed())
			throw new IllegalStateException("Schema must be sealed before records based on it can be created!");
		this.schema = schema;
		values = new Object[schema.getNumberOfColumns(false)];
	}
	
	/**
	 * Creates an initialised record
	 * 
	 * @param schema
	 * @param values to initialise record, number of values must match number of (real) columns in the schema and each value must be valid for the corresponding column
	 */
	protected Record(Schema schema, Object... values)
	{
		this(schema);
		if(values != null)
		{	
			if(this.values.length == values.length)
			{
				// Init from given values:
				for(int c = 0; c < this.values.length; c++)
				{
					Column<?> col = schema.getColumn(c);
					col.storeObject(this, values[c]);
				}
			}
			else
				throw new IllegalArgumentException("Unexpected number of values (given: " + values.length + "; expected: " + this.values.length + ").");
		}
	}
	
	/**
	 * Creates an initialised record
	 * 
	 * @param schema
	 * @param serialisedValues String to initialise record (should not contain values of virtual columns, i.e. the String must be as produced by {@link #serialise()})
	 * @throws Exception 
	 */
	protected Record(Schema schema, String serialisedValues) throws Exception
	{
		this(schema);
		parse(serialisedValues);
	}

	/**
	 * Creates an initialised record
	 * 
	 * @param schema
	 * @param serialisedValues byte array to initialise record (should not contain values of virtual columns, i.e. the String must be as produced by {@link #toBytes()})
	 * @throws NullPointerException when schema is null
	 * @throws IOException when reading serialisedValues fails
	 */
	protected Record(Schema schema, byte[] serialisedValues) throws NullPointerException, IOException
	{
		this(schema);
		fromBytes(serialisedValues);
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param another
	 */
	public Record(Record another)
	{
		this(another.schema);
		
		//(Deep) copy of values:
		for(int c = 0; c < this.values.length; c++)
		{
			Column<?> col = schema.getColumn(c);
			this.values[c] = col.copyObject(another.values[c]);
		}
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
	 * @deprecated probably unsafe, avoid this unless there is a very good reason and you know what you are doing
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
	 * @deprecated probably unsafe, avoid this unless there is a very good reason and you know what you are doing
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
	 * @param value the value to set
	 * @throws IllegalArgumentException when the column does not exist in the record's schema, because it is virtual, or because it is incompatible with the schema column by the same name
	 */
	protected void setValue(Column<?> column, Object value) throws IllegalArgumentException
	{
		values[getPosition(column)] = value; // set value in array
	}
	
	/**
	 * To be called from {@link Column#retrieveValue(Record)}
	 * 
	 * @param column
	 * @param the current value
	 * @throws IllegalArgumentException when the column does not exist in the record's schema, because it is virtual, or because it is incompatible with the schema column by the same name
	 */
	protected Object getValue(Column<?> column) throws IllegalArgumentException
	{
		return values[getPosition(column)]; // return value from array
	}
	
	/**
	 * Returns the index (= position) at which the given column's value can be found in the values array.
	 * 
	 * @param column
	 * @return values array index
	 * @throws IllegalArgumentException when the column does not exist in the record's schema, because it is virtual, or because it is incompatible with the schema column by the same name
	 */
	private int getPosition(Column<?> column) throws IllegalArgumentException
	{
		// Get column position by its name:
		int position = schema.getColumnPosition(column.name);
		// Check position:
		if(position == Schema.UNKNOWN_COLUMN_POSITION)
		{
			if(column instanceof VirtualColumn)
				throw new IllegalArgumentException("Records do not hold values of virtual columns!"); // this should never happen because VirtualColumn overrides Column#retrieveValue(Record) 
			else
				throw new IllegalArgumentException("The schema of this record has no such column (\"" + column.name + "\").");
		}
		// Compatibility check:
		Column<?> schemaColumn = schema.getColumn(position);
		if(column != schemaColumn && !column.isCompatible(schemaColumn))
			throw new IllegalArgumentException("Schema mismatch: incompatible column.");
		// All OK, return position:
		return position;
	}

	/**
	 * Checks whether all non-optional columns have been assigned a (non-null) value in this record.
	 * 
	 * @return whether of the all non-optional columns are filled
	 */
	public boolean isFilled()
	{
		for(int c = 0; c < values.length; c++)
			if(values[c] == null && !schema.getColumn(c).isOptional())
				return false; // null value in non-optional column	
		return true;
	}
	
	/**
	 * Checks whether all non-optional columns from the given schema have been assigned a (non-null) value in this record.
	 * The given schema must be a subset of the record's schema or the record's schema itself.
	 * This method was added for the purpose of checking whether primary keys (which are a subset of a schema's columns) have been set.
	 * 
	 * @param schema (subset of) the record's schema
	 * @return whether of the all non-optional columns are filled
	 * @throws IllegalArgumentException when the given schema contains a column(s) which is not part of the record's schema
	 */
	protected boolean isFilled(Schema schema) throws IllegalStateException
	{
		for(Column<?> col : schema.getColumns(false))
			if(getValue(col) == null && !col.isOptional())
				return false; // null value in non-optional column	
		return true;
	}
	
	/**
	 * Returns a reference to the record. Only works if the schema has a primary key.
	 * 
	 * @return a {@link RecordReference} instance pointing to this record
	 * @throws NullPointerException	if the Schema of this Record does not have a primary key
	 */
	public RecordReference getReference() throws NullPointerException
	{
		return new RecordReference(this);
	}
	
	/**
	 * Returns a {@link SingleRecordQuery} which can be used to find this record in a RecordStore or Collection, by matching the primary key (and no other columns!).
	 * 
	 * @return a query that looks for this record
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	public SingleRecordQuery getRecordQuery() throws IllegalStateException
	{
		return new FirstRecordQuery(Source.From(schema), Order.UNDEFINED, getRecordQueryConstraint());
	}
	
	/**
	 * Returns a {@link Constraint} that matches on the Record's primary key values.
	 * 
	 * @return
	 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
	 */
	public Constraint getRecordQueryConstraint() throws IllegalStateException
	{
		if(!isFilled(schema.getPrimaryKey()))
			throw new IllegalStateException("All values of the key must be set before a record selecting contraint/query can be created!");
		
		// Match for key parts:
		AndConstraint constraints = new AndConstraint();
		for(Column<?> keyPartCol : schema.getPrimaryKey().getColumns(false))
			constraints.addConstraint(new EqualityConstraint(keyPartCol, getValue(keyPartCol)));
		
		return constraints.reduce();
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
	 * Serialise the Record to a String, excluding virtual columns
	 * 
	 * @return
	 */
	public String serialise()
	{
		return serialise(false, Collections.<Column<?>> emptySet());
	}
	
	/**
	 * Serialise the Record to a String
	 * 
	 * @param includeVirtual
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
			if(!skipColumns.contains(col))
			{
				String valueString = col.retrieveValueAsString(this);
				bldr.append(valueString == null ? "" : StringUtils.escape(valueString, SERIALISATION_SEPARATOR, SERIALISATION_SEPARATOR_ESCAPE, SERIALISATION_SEPARATOR_ESCAPE_PREFIX));
			}
		}
		return bldr.toString();
	}
	
	/**
	 * Deserialise the values of a Record from a String, not expecting virual columns
	 * 
	 * @param serialisedRecord
	 * @throws Exception
	 * @return the Record itself
	 */
	public Record parse(String serialisedRecord) throws Exception
	{
		return parse(serialisedRecord, false, Collections.<Column<?>> emptySet());
	}
	
	/**
	 * Deserialise the values of a Record from a String
	 * 
	 * @param serialisedRecord
	 * @param includeVirtual
	 * @param skipColumns
	 * @return
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
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
	
	/**
	 * Serialise the Record to a byte array, excluding virtual columns
	 * 
	 * @return
	 * @throws IOException
	 */
	public byte[] toBytes() throws IOException
	{
		BitOutputStream out = null;
		try
		{
			// Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitWrapOutputStream(rawOut);
				
			// Write record:
			this.writeToBitStream(out, false, Collections.<Column<?>> emptySet());
			
			// Flush & close the stream and get bytes:
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
	 * @param bitStream
	 * @param includeVirtual
	 * @param skipColumns
	 * @throws IOException
	 */
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
	
	/**
	 * Deserialise the Record from a byte array, excluding virtual columns
	 * 
	 * @param bytes
	 * @return the record itself
	 * @throws IOException
	 */
	public Record fromBytes(byte[] bytes) throws IOException
	{
		BitInputStream in = null;
		try
		{
			// Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(bytes);
			in = new BitWrapInputStream(rawIn);
				
			// Read record:
			this.readFromBitStream(in, false, Collections.<Column<?>> emptySet());
		}
		catch(Exception e)
		{
			throw new IOException("Error on decoding record.", e);
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(Exception ignore) {}
		}
		return this;
	}
	
	/**
	 * @param bitStream
	 * @param includeVirtual
	 * @param skipColumns
	 * @throws IOException
	 */
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
			out = new BitWrapOutputStream(new ByteArrayOutputStream());
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
	
	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + schema.hashCode();
		hash = 31 * hash + Arrays.deepHashCode(values);
		return hash;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		return equals(obj, true, false);
	}
	
	/**
	 * @param obj
	 * @param checkSchema
	 * @param asStoredBinary whether or not to compare values as if they've been writen/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean equals(Object obj, boolean checkSchema, boolean asStoredBinary)
	{
		if(this == obj)
			return true;
		if(obj instanceof Record)
		{
			Record other = (Record) obj;
			if(checkSchema)
			{	// Check if records have the same (or 100% equivalent) schema
				if(!this.schema.equals(other.schema, true, true, false)) // there's no point in checking indexes for this purpose
					return false;
			}
			else
			{	// Only check if the number of columns matches (to avoid out or range errors below):
				if(this.schema.getNumberOfColumns(false) != other.schema.getNumberOfColumns(false))
					return false;
			}
			// Compare values for each column:
			return hasEqualValues(other, asStoredBinary);
		}
		else
			return false;
	}
	
	/**
	 * Compare the values of this record with those of another.
	 * 
	 * @param other
	 * @return
	 */
	public boolean hasEqualValues(Record other)
	{
		return hasEqualValues(other, false);
	}
	
	/**
	 * Compare the values of this record with those of another.
	 * If {@code asStoredBinary} is {@code true} the records must be of the same schema, otherwise an exception will be thrown.
	 * 
	 * @param other
	 * @param asStoredBinary whether or not to compare values as if they've been writen/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean hasEqualValues(Record other, boolean asStoredBinary)
	{
		return other == null ?	false :
								(asStoredBinary ?
									hasEqualValues(other, this.schema.getColumns(false), true) : // compare all non-virtual columns, using values as if decoded from binary stream
									Arrays.deepEquals(this.values, other.values));
	}
	
	/**
	 * Compare the values of this record with those of another, across the given list of columns.
	 * This and the other record as assumed to have schemata that are the same or at least each share the given columns (or equivalents).
	 * 
	 * @param other
	 * @param columns
	 * @param asStoredBinary whether or not to compare values as if they've been written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	public boolean hasEqualValues(Record other, List<Column> columns, boolean asStoredBinary)
	{
		if(other == null)
			return false;
		for(Column c : columns)
		{
			if(!EqualValues(asStoredBinary ? c.retrieveValueAsStoredBinary(this) : c.retrieveValue(this),
							asStoredBinary ? c.retrieveValueAsStoredBinary(other) : c.retrieveValue(other)))
				return false;
		}
		return true;
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
