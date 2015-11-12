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
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.StreamHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.types.Location;

/**
 * An ordered set of values, each corresponding to a (non-virtual) {@link Column} of a {@link ColumnSet}.
 * Superclass for {@link Record}, {@link RecordReference}, {@link Location}, etc.
 * 
 * @author mstevens
 *
 * @param <CS> the {@link ColumnSet} type
 */
public class ValueSet<CS extends ColumnSet> implements Serializable
{
	
	// Statics-------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static final private char SERIALISATION_SEPARATOR = ',';
	static final private char SERIALISATION_SEPARATOR_ESCAPE = '.';
	static final private char SERIALISATION_SEPARATOR_ESCAPE_PREFIX = '/';
	
	// Dynamics------------------------------------------------------
	protected /*final*/ CS columnSet; // not final (for now) for Record#setSchema() methods
	
	protected final Object[] values;
	
	/**
	 * Creates a new, ValueSet with the given ColumnSet, with each Column's value set to its defaultValue (usually {@code null}).
	 * 
	 * @param columnSet
	 */
	public ValueSet(CS columnSet)
	{
		if(columnSet == null)
			throw new NullPointerException("Schema cannot be null!");
		if(!columnSet.isSealed())
			throw new IllegalStateException("Schema must be sealed before records based on it can be created!");
		this.columnSet = columnSet;
		values = new Object[columnSet.getNumberOfColumns(false)];
		// Initialise values with the defaultValue of each Column (usually null):
		for(int c = 0; c < this.values.length; c++)
			this.values[c] = columnSet.getColumn(c).defaultValue;
	}
	
	/**
	 * Creates an initialised ValueSet.
	 * 
	 * @param columnSet
	 * @param values to initialise the ValueSet with, number of values must match number of (real) columns in the ColumnSet and each value must be valid for the corresponding Column
	 * @throws IllegalArgumentException in case of an incorrect number of values or an invalid value
	 * @throws NullPointerException if a value is null on an non-optional column
	 * @throws ClassCastException when a value cannot be converted/casted to the column's type {@code <T>}
	 */
	public ValueSet(CS columnSet, Object... values) throws IllegalArgumentException, NullPointerException, ClassCastException
	{
		this(columnSet);
		if(values != null)
		{	
			if(this.values.length == values.length)
			{
				// Init from given values:
				for(int c = 0; c < this.values.length; c++)
					columnSet.getColumn(c).storeObject(this, values[c]); // validation (and possibly conversion) will be applied
			}
			else
				throw new IllegalArgumentException("Unexpected number of values (given: " + values.length + "; expected: " + this.values.length + ").");
		}
	}
	
	/**
	 * Creates an initialised ValueSet.
	 * 
	 * @param columnSet
	 * @param serialisedValues String to initialise ValueSet with (should not contain values of virtual columns, i.e. the String must be as produced by {@link #serialise()})
	 * @throws Exception when parsing serialisedValues fails (or they are invalid)
	 */
	public ValueSet(CS columnSet, String serialisedValues) throws Exception
	{
		this(columnSet);
		parse(serialisedValues); // validation will be applied
	}

	/**
	 * Creates an initialised ValueSet.
	 * 
	 * @param columnSet
	 * @param serialisedValues byte array to initialise ValueSet with (should not contain values of virtual columns, i.e. the String must be as produced by {@link #toBytes()})
	 * @throws NullPointerException when schema is null
	 * @throws IOException when reading serialisedValues fails (or they are invalid)
	 */
	public ValueSet(CS columnSet, byte[] serialisedValues) throws NullPointerException, IOException
	{
		this(columnSet);
		fromBytes(serialisedValues); // validation will be applied
	}
	
	/**
	 * Copy constructor. Creates an initialised ValueSet with the values of another ValueSet.
	 * 
	 * @param another
	 */
	public ValueSet(ValueSet<CS> another)
	{
		this(another.columnSet);
		
		//(Deep) copy of values:
		for(int c = 0; c < this.values.length; c++)
		{
			Column<?> col = columnSet.getColumn(c);
			this.values[c] = col.copyObject(another.values[c]);
		}
	}
	
	/**
	 * @return the columnSet
	 */
	public CS getColumnSet()
	{
		return columnSet;
	}
	
	/**
	 * To be called from {@link Column#storeValue(Record, Object)}.
	 * This method is not {@code final} for the sake of the {@link UnmodifiableValueSet} subclass.
	 * 
	 * @param column
	 * @param value the value to set (may be null, e.g. to clear earlier values)
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
	protected final Object getValue(Column<?> column) throws IllegalArgumentException
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
	protected final int getPosition(Column<?> column) throws IllegalArgumentException
	{
		// Get column position by its name:
		int position = columnSet.getColumnPosition(column.name);
		// Check position:
		if(position == Schema.UNKNOWN_COLUMN_POSITION)
		{
			if(column instanceof VirtualColumn)
				throw new IllegalArgumentException("Records do not hold values of virtual columns!"); // this should never happen because VirtualColumn overrides Column#retrieveValue(Record) 
			else
				throw new IllegalArgumentException("The schema of this record has no such column (\"" + column.name + "\").");
		}
		// Compatibility check:
		Column<?> schemaColumn = columnSet.getColumn(position);
		if(column != schemaColumn && !column.isCompatible(schemaColumn))
			throw new IllegalArgumentException("Schema mismatch: incompatible column.");
		// All OK, return position:
		return position;
	}

	/**
	 * Checks whether all non-optional columns have been assigned a non-{@code null} value in this ValueSet.
	 * 
	 * @return whether all of the non-optional columns are filled
	 * 
	 * @see {@link Column#isValuePresentOrOptional(ValueSet)}
	 */
	public boolean isFilled()
	{
		return isFilled(false); // don't recurse by default
	}
	
	/**
	 * Checks whether all non-optional columns have been assigned a non-{@code null} value in this ValueSet.
	 * 
	 * @param skipColumns a set of columns to ignore in this check
	 * @return whether all of the non-optional columns are filled
	 * 
	 * @see {@link Column#isValuePresentOrOptional(ValueSet)}
	 */
	public boolean isFilled(Set<? extends Column<?>> skipColumns)
	{
		return isFilled(skipColumns, false); // don't recurse by default
	}
	
	/**
	 * Checks, optionally recursive whether all non-optional columns have been assigned a non-{@code null} value in this ValueSet.
	 * 
	 * @param recurse whether or not to recursively check whether the non-optional subcolumns of composite columns also have a non-{@code null} value
	 * @return whether all of the non-optional columns are filled
	 * 
	 * @see {@link Column#isValuePresentOrOptional(ValueSet,boolean)}
	 */
	public boolean isFilled(final boolean recurse)
	{
		return isFilled(this.columnSet, Collections.<Column<?>> emptySet(), recurse);
	}
	
	/**
	 * Checks, optionally recursive whether all non-optional columns have been assigned a non-{@code null} value in this ValueSet.
	 * 
	 * @param skipColumns a set of columns to ignore in this check
	 * @param recurse whether or not to recursively check whether the non-optional subcolumns of composite columns also have a non-{@code null} value
	 * @return whether all of the non-optional columns are filled
	 * 
	 * @see {@link Column#isValuePresentOrOptional(ValueSet,boolean)}
	 */
	public boolean isFilled(Set<? extends Column<?>> skipColumns, final boolean recurse)
	{
		return isFilled(this.columnSet, skipColumns, recurse);
	}
	
	/**
	 * Checks whether all non-optional columns from the given schema have been assigned a non-{@code null} value in this ValueSet.
	 * The given ColumnSet must be this ValueSet's ColumnSet or a subset of it.
	 * This method was added for the purpose of checking whether primary keys (which are a subset of a schema's columns) have been set.
	 * 
	 * @param columnSet (subset of) the record's schema
	 * @return whether all of the non-optional columns are filled
	 * @throws IllegalArgumentException when the given ColumnSet contains a column(s) which is not part of the ValueSet's ColumnSet
	 * 
	 * @see {@link Column#isValuePresentOrOptional(ValueSet)}
	 */
	protected boolean isFilled(ColumnSet columnSet) throws IllegalStateException
	{
		return isFilled(columnSet, Collections.<Column<?>> emptySet(), false); // don't recurse by default
	}
	
	/**
	 * Checks, optionally recursive, whether all non-optional columns from the given schema have been assigned a non-{@code null} value in this ValueSet.
	 * The given ColumnSet must be this ValueSet's ColumnSet or a subset of it.
	 * This method was added for the purpose of checking whether primary keys (which are a subset of a schema's columns) have been set.
	 * 
	 * @param columnSet (subset of) the record's schema
	 * @param skipColumns a set of columns to ignore in this check
	 * @param recurse whether or not to recursively check whether the non-optional subcolumns of composite columns also have a non-{@code null} value
	 * @return whether all of the non-optional columns are filled
	 * @throws IllegalArgumentException when the given ColumnSet contains a column(s) which is not part of the ValueSet's ColumnSet
	 * 
	 * @see {@link Column#isValuePresentOrOptional(ValueSet,boolean)}
	 */
	protected boolean isFilled(ColumnSet columnSet, Set<? extends Column<?>> skipColumns, final boolean recurse) throws IllegalStateException
	{
		for(Column<?> col : columnSet.getColumns(false))
			if(!skipColumns.contains(col) && !col.isValuePresentOrOptional(this, recurse))
				return false; // null value in non-optional column	
		return true;
	}
	
	/**
	 * Checks whether this ValueSet has a valid value for each of the (non-virtual) columns in its ColumnSet.
	 * Note that is it should not actually be possible to have stored invalid values in the ValueSet.
	 * 
	 * @return whether there is a valid value for each column
	 * 
	 * @see {@link Column#isValueValid(ValueSet)}
	 */
	public boolean isValid()
	{
		return isValid(false); // don't recurse by default
	}
	
	/**
	 * Checks whether this ValueSet has a valid value for each of the (non-virtual) columns in its ColumnSet.
	 * Note that is it should not actually be possible to have stored invalid values in the ValueSet.
	 * 
	 * @param skipColumns a set of columns to ignore in this check
	 * @return whether there is a valid value for each column
	 * 
	 * @see {@link Column#isValueValid(ValueSet)}
	 */
	public boolean isValid(Set<? extends Column<?>> skipColumns)
	{
		return isValid(skipColumns, false); // don't recurse by default
	}
	
	/**
	 * Checks whether this ValueSet has a valid value for each of the (non-virtual) columns in its ColumnSet.
	 * Note that is it should not actually be possible to have stored invalid values in the ValueSet.
	 * 
	 * @param recurse whether or not to recursively check whether the subcolumns of composite columns also have valid values
	 * @return whether there is a valid value for each column
	 * 
	 * @see {@link Column#isValueValid(ValueSet,boolean)}
	 */
	public boolean isValid(final boolean recurse)
	{
		return isValid(Collections.<Column<?>> emptySet(), recurse);
	}
	
	/**
	 * Checks whether this ValueSet has a valid value for each of the (non-virtual) columns in its ColumnSet.
	 * Note that is it should not actually be possible to have stored invalid values in the ValueSet.
	 * 
	 * @param skipColumns a set of columns to ignore in this check
	 * @param recurse whether or not to recursively check whether the subcolumns of composite columns also have valid values
	 * @return whether there is a valid value for each column
	 * 
	 * @see {@link Column#isValueValid(ValueSet,boolean)}
	 */
	public boolean isValid(Set<? extends Column<?>> skipColumns, final boolean recurse)
	{
		for(Column<?> col : columnSet.getColumns(false))
			if(!skipColumns.contains(col) && !col.isValueValid(this, recurse))
				return false;	
		return true;
	}
	
	@Override
	public String toString()
	{
		return toString(true);
	}
	
	public String toString(boolean includeVirtual)
	{
		TransactionalStringBuilder bldr = new TransactionalStringBuilder("");
		// ValueSet type:
		bldr.append(getClass().getSimpleName());
		// ColumnSet (type+name):
		bldr.append("<" + columnSet.toString() + ">:[");
		// Values:
		bldr.openTransaction("; ");
		for(Column<?> c : columnSet.getColumns(includeVirtual))
			bldr.append(c.getName() + " = " + c.retrieveValueAsString(this));
		bldr.commitTransaction();
		bldr.append("]");
		// Return result:
		return bldr.toString();
	}
	
	/**
	 * Serialise the ValueSet to a String, excluding virtual columns
	 * 
	 * @return
	 */
	public String serialise()
	{
		return serialise(false, Collections.<Column<?>> emptySet());
	}
	
	/**
	 * Serialise the ValueSet to a String
	 * 
	 * @param includeVirtual
	 * @param skipColumns
	 * @return
	 */
	public String serialise(boolean includeVirtual, Set<? extends Column<?>> skipColumns)
	{
		StringBuilder bldr = new StringBuilder();
		boolean first = true;
		for(Column<?> col : columnSet.getColumns(includeVirtual))
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
	 * Deserialise the values of a ValueSet from a String, not expecting virtual columns
	 * 
	 * @param serialisedRecord
	 * @throws Exception
	 * @return the Record itself
	 */
	public ValueSet<CS> parse(String serialisedRecord) throws Exception
	{
		return parse(serialisedRecord, false, Collections.<Column<?>> emptySet());
	}
	
	/**
	 * Deserialise the values of a ValueSet from a String.
	 * 
	 * Note:
	 * 	Sealed ColumnSets always have at least 1 column, which means that a serialisedValueSet should always
	 * 	contain at least one value. The separator is only used in between values and not at the end of the String
	 *	(see {@link #serialise()}), and thus the number of values is equal to the number of separator occurrences + 1.
	 * 
	 * @param serialisedValueSet
	 * @param includeVirtual
	 * @param skipColumns
	 * @return
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	public ValueSet<CS> parse(String serialisedValueSet, boolean includeVirtual, Set<? extends Column<?>> skipColumns) throws ParseException, IllegalArgumentException, NullPointerException
	{
		String[] parts = serialisedValueSet.split("\\" + SERIALISATION_SEPARATOR);  
		if(parts.length != columnSet.getNumberOfColumns(includeVirtual))
			throw new IllegalArgumentException("Unexpected number of values (got: " + parts.length + "; expected: " + columnSet.getNumberOfColumns(includeVirtual) + ") in serialised record (" + serialisedValueSet +  ").");
		int p = 0;
		for(Column<?> col : columnSet.getColumns(includeVirtual))
		{
			if(!(col instanceof VirtualColumn) && !skipColumns.contains(col)) // skip virtual columns & skipColumns (but *do* increment the counter p!)
				col.parseAndStoreValue(this, StringUtils.deescape(parts[p], SERIALISATION_SEPARATOR, SERIALISATION_SEPARATOR_ESCAPE, SERIALISATION_SEPARATOR_ESCAPE_PREFIX)); // validation will be performed
			p++;
		}
		return this;
	}
	
	/**
	 * Serialise the ValueSet to a byte array, excluding virtual columns.
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
			this.writeToBitStream(out);
			
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
			StreamHelpers.SilentClose(out);
		}
	}
	
	/**
	 * Write all ValueSet values to the given bitStream, excluding those of virtual columns.
	 * 
	 * @param bitStream
	 * @throws IOException
	 */
	public void writeToBitStream(BitOutputStream bitStream) throws IOException
	{
		writeToBitStream(bitStream, columnSet.getColumns(false), Collections.<Column<?>> emptySet());
	}
	
	/**
	 * Write ValueSet values, possibly including those of virtual columns and except the given skipped ones, to the given bitStream.
	 * 
	 * @param bitStream
	 * @param includeVirtual whether or not to include the values corresponding to virtual columns
	 * @param skipColumns columns *not* to include the values of
	 * @throws IOException
	 */
	public void writeToBitStream(BitOutputStream bitStream, boolean includeVirtual, Set<? extends Column<?>> skipColumns) throws IOException
	{
		writeToBitStream(bitStream, columnSet.getColumns(includeVirtual), skipColumns);
	}
	
	/**
	 * Write ValueSet values of the given columns (in given order) to the given bitStream.
	 * 
	 * @param bitStream
	 * @param columns columns to include the values of
	 * @throws IOException
	 */
	public void writeToBitStream(BitOutputStream bitStream, List<? extends Column<?>> columns) throws IOException
	{
		writeToBitStream(bitStream, columns, Collections.<Column<?>> emptySet());
	}
	
	/**
	 * Write ValueSet values of the given columns (in given order), except the given skipped ones, to the given bitStream.
	 * 
	 * @param bitStream
	 * @param columns columns to include the values of
	 * @param skipColumns columns *not* to include the values of
	 * @throws IOException
	 */
	public void writeToBitStream(BitOutputStream bitStream, List<? extends Column<?>> columns/*, boolean includeVirtual*/, Set<? extends Column<?>> skipColumns) throws IOException
	{
		try
		{	// Write fields:
			for(Column<?> c : columns)
				if(/*(includeVirtual || !(c instanceof VirtualColumn)) && */!skipColumns.contains(c))
					c.retrieveAndWriteValue(this, bitStream);
		}
		catch(Exception e)
		{
			throw new IOException("Error on attempting to write record", e);
		}
	}
	
	/**
	 * Deserialise the ValueSet from a byte array, excluding virtual columns.
	 * 
	 * @param bytes
	 * @return the record itself
	 * @throws IOException
	 */
	public ValueSet<CS> fromBytes(byte[] bytes) throws IOException
	{
		BitInputStream in = null;
		try
		{
			// Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(bytes);
			in = new BitWrapInputStream(rawIn);
				
			// Read record:
			this.readFromBitStream(in);
		}
		catch(Exception e)
		{
			throw new IOException("Error on decoding record.", e);
		}
		finally
		{
			StreamHelpers.SilentClose(in);
		}
		return this;
	}
	
	/**
	 * Read all ValueSet values from the given bitStream, excluding those of virtual columns.
	 * 
	 * @param bitStream
	 * @throws IOException
	 */
	public void readFromBitStream(BitInputStream bitStream) throws IOException
	{
		readFromBitStream(bitStream, columnSet.getColumns(false), Collections.<Column<?>> emptySet());
	}
	
	/**
	 * Read ValueSet values, possibly including those of virtual columns and except the given skipped ones, from the given bitStream.
	 * 
	 * @param bitStream
	 * @param includeVirtual whether or not to expect the values corresponding to virtual columns, if {@code true} these values will be read from the stream but not stored (i.e. they will effectively be skipped)
	 * @param skipColumns columns *not* to expect the values of
	 * @throws IOException
	 */
	public void readFromBitStream(BitInputStream bitStream, boolean includeVirtual, Set<? extends Column<?>> skipColumns) throws IOException
	{
		readFromBitStream(bitStream, columnSet.getColumns(includeVirtual), skipColumns);
	}
	
	/**
	 * Read ValueSet values of the given columns (in given order) from the given bitStream.
	 * 
	 * @param bitStream
	 * @param columns columns to expect the values of
	 * @throws IOException
	 */
	public void readFromBitStream(BitInputStream bitStream, List<? extends Column<?>> columns) throws IOException
	{
		readFromBitStream(bitStream, columns, Collections.<Column<?>> emptySet());
	}
	
	/**
	 * Read ValueSet values of the given columns (in given order), except the given skipped ones, from the given bitStream.
	 * 
	 * @param bitStream
	 * @param columns columns to except the values of
	 * @param skipColumns columns *not* to except the values of
	 * @throws IOException
	 */
	public void readFromBitStream(BitInputStream bitStream, List<? extends Column<?>> columns/*, boolean includeVirtual*/, Set<? extends Column<?>> skipColumns) throws IOException
	{
		try
		{	// Read fields:
			for(Column<?> c : columns)
				if(/*(includeVirtual || !(c instanceof VirtualColumn)) && */!skipColumns.contains(c))
				{
					if(c instanceof VirtualColumn)
						c.readValue(bitStream); // read but don't store values of virtual columns (i.e. we skip them in the stream)
					else
						c.readAndStoreValue(this, bitStream);
				}
		}
		catch(Exception e)
		{
			throw new IOException("Error on attempting to read record. Read so far: " + this.toString(), e);
		}
	}
	
	/**
	 * Gets the size of this ValueSet in number of bits
	 * 
	 * @return
	 */
	public int getSize(boolean includeVirtual, Set<? extends Column<?>> skipColumns)
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
			StreamHelpers.SilentClose(out);
		}
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + columnSet.hashCode();
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
	 * @param checkColumnSet
	 * @param asStoredBinary whether or not to compare values as if they've been writen/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean equals(Object obj, boolean checkColumnSet, boolean asStoredBinary)
	{
		if(this == obj)
			return true;
		if(obj instanceof ValueSet)
		{
			ValueSet<?> other = (ValueSet<?>) obj;
			if(checkColumnSet)
			{	// Check if records have the same (or 100% equivalent) schema
				if(!this.columnSet.equals(other.columnSet, true, true)) // there's no point in checking indexes for this purpose
					return false;
			}
			else
			{	// Only check if the number of columns matches (to avoid out or range errors below):
				if(this.columnSet.getNumberOfColumns(false) != other.columnSet.getNumberOfColumns(false))
					return false;
			}
			// Compare values for each column:
			return hasEqualValues(other, asStoredBinary);
		}
		else
			return false;
	}
	
	/**
	 * Compare the values of this ValueSet with those of another.
	 * 
	 * @param other
	 * @return
	 */
	public boolean hasEqualValues(ValueSet<CS> other)
	{
		return hasEqualValues(other, Collections.<Column<?>> emptySet(), false);
	}
	
	/**
	 * Compare the values of this ValueSet with those of another.
	 * 
	 * @param other
	 * @param skipColumns ignore these columns
	 * @return
	 */
	public boolean hasEqualValues(ValueSet<?> other, Set<? extends Column<?>> skipColumns)
	{
		return hasEqualValues(other, skipColumns, false);
	}
	
	/**
	 * Compare the values of this ValueSet with those of another.
	 * If {@code asStoredBinary} is {@code true} the ValueSets must be of the same schema, otherwise an exception will be thrown.
	 * 
	 * @param other another ValueSet
	 * @param asStoredBinary whether or not to compare values as if they've been written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean hasEqualValues(ValueSet<?> other, boolean asStoredBinary)
	{
		return hasEqualValues(other, Collections.<Column<?>> emptySet(), asStoredBinary);
	}

	/**
	 * Compare the values of this ValueSet with those of another.
	 * If {@code asStoredBinary} is {@code true} the records must be of the same schema, otherwise an exception will be thrown.
	 * 
	 * @param other another ValueSet
	 * @param skipColumns ignore these columns
	 * @param asStoredBinary whether or not to compare values as if they've been written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean hasEqualValues(ValueSet<?> other, Set<? extends Column<?>> skipColumns, boolean asStoredBinary)
	{
		return other == null ?	false :
								(!skipColumns.isEmpty() || asStoredBinary ?
									hasEqualValues(other, this.columnSet.getColumns(false), skipColumns, asStoredBinary) :
									this == other || Arrays.deepEquals(this.values, other.values));
	}
	
	/**
	 * Compare the values of this ValueSet with those of another, across the given collection of columns.
	 * This and the other record as assumed to have schemata that are the same or at least each share the given columns (or equivalents).
	 * 
	 * @param other another ValueSet
	 * @param columns the columns that will be checked, unless they appear in skipColumns
	 * @param skipColumns
	 * @param asStoredBinary whether or not to compare values as if they've been written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean hasEqualValues(ValueSet<?> other, Collection<? extends Column<?>> columns, Set<? extends Column<?>> skipColumns, boolean asStoredBinary)
	{
		if(other == null)
			return false;
		if(this != other)
			for(Column<?> c : columns)
				if(	!skipColumns.contains(c) &&
					!Objects.deepEquals(asStoredBinary ? c.retrieveValueAsStoredBinary(this) : c.retrieveValue(this),
										asStoredBinary ? c.retrieveValueAsStoredBinary(other) : c.retrieveValue(other)))
					return false;
		return true;
	}
	
}
