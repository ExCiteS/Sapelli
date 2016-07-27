/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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
import java.util.Iterator;
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
import uk.ac.ucl.excites.sapelli.storage.model.columns.LosslessFlagColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Location;
import uk.ac.ucl.excites.sapelli.storage.util.InvalidColumnException;
import uk.ac.ucl.excites.sapelli.storage.util.InvalidValueException;

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
	
	static final public char SERIALISATION_SEPARATOR = ',';
	static final public char DEFAULT_SERIALISATION_DELIMITER = '`';
	
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
	 * @throws IllegalArgumentException in case of an incorrect number of values
	 * @throws InvalidValueException in case of an invalid value
	 * @throws NullPointerException if a value is null on an non-optional column
	 * @throws ClassCastException when a value cannot be converted/casted to the column's type {@code <T>}
	 */
	public ValueSet(CS columnSet, Object... values) throws IllegalArgumentException, InvalidValueException, NullPointerException, ClassCastException
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
	 * @param serialisedValues byte array to initialise ValueSet with (should not contain values of virtual columns and may or may not be {@code lossless}ly encoded, i.e. the array must be as produced by {@code #toBytes(lossless)})
	 * @param lossless whether the given byte array is a (guaranteed) lossless ({@code true}), or a (possibly) lossy ({@code false}) representation of the values
	 * @throws NullPointerException when schema is null
	 * @throws IOException when reading serialisedValues fails (or they are invalid)
	 */
	public ValueSet(CS columnSet, byte[] serialisedValues, boolean lossless) throws NullPointerException, IOException
	{
		this(columnSet);
		fromBytes(serialisedValues, lossless); // validation will be applied
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
			this.values[c] = col.copyObject(another.values[c], false); // cast, don't convert
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
	 * @throws InvalidColumnException when the column does not exist in the record's schema, because it is virtual, or because it is incompatible with the schema column by the same name
	 */
	protected void setValue(Column<?> column, Object value) throws InvalidColumnException
	{
		values[getPosition(column)] = value; // set value in array
	}
	
	/**
	 * To be called from {@link Column#retrieveValue(Record)}
	 * 
	 * @param column
	 * @param the current value
	 * @throws InvalidColumnException when the column does not exist in the record's schema, because it is virtual, or because it is incompatible with the schema column by the same name
	 */
	protected final Object getValue(Column<?> column) throws InvalidColumnException
	{
		return values[getPosition(column)]; // return value from array
	}
	
	/**
	 * (Re-)sets the values of all columns {@code null}, even if the column is non-optional or has a non-{@code null} {@link Column#defaultValue}.
	 * Use with care!
	 * 
	 * @see Column#clearValue(ValueSet)
	 */
	public void clear()
	{
		for(Column<?> col : columnSet.getColumns(false))
			col.clearValue(this);
	}
	
	/**
	 * Resets the values of all columns to the {@link Column#defaultValue} (usually {@code null}), even if the column is non-optional.
	 * Use with care!
	 * 
	 * @see Column#resetValue(ValueSet)
	 */
	public void reset()
	{
		for(Column<?> col : columnSet.getColumns(false))
			col.resetValue(this);
	}
	
	/**
	 * Returns the index (= position) at which the given column's value can be found in the values array.
	 * 
	 * @param column
	 * @return values array index
	 * @throws InvalidColumnException when the column does not exist in the record's schema, because it is virtual, or because it is incompatible with the schema column by the same name
	 */
	protected final int getPosition(Column<?> column) throws InvalidColumnException
	{
		// Get column position by its name:
		int position = columnSet.getColumnPosition(column.name);
		// Check position:
		if(position == Schema.UNKNOWN_COLUMN_POSITION)
		{
			if(column instanceof VirtualColumn)
				throw new InvalidColumnException("ValueSets do not hold values of virtual columns!", column); // this should never happen because VirtualColumn overrides Column#retrieveValue(Record) 
			else
				throw new InvalidColumnException("The columnSet has no such column (" + (column != null ? "\"" + column.name + "\"" : "null") + ").", column);
		}
		// Compatibility check:
		Column<?> schemaColumn = columnSet.getColumn(position);
		if(column != schemaColumn && !column.isCompatible(schemaColumn))
			throw new InvalidColumnException("Schema mismatch: incompatible column.", column);
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
		return isFilled(this.columnSet, ColumnSet.SKIP_NONE, recurse);
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
	 * Checks whether all non-optional columns from the given columnSet have been assigned a non-{@code null} value in this ValueSet.
	 * The given ColumnSet must be this ValueSet's ColumnSet or a subset of it.
	 * This method was added for the purpose of checking whether primary keys (which are a subset of a schema's columns) have been set.
	 * 
	 * @param columnSet (subset of) the record's schema
	 * @return whether all of the non-optional columns are filled
	 * @throws InvalidColumnException when the given ColumnSet contains a column(s) which is not part of the ValueSet's ColumnSet
	 * 
	 * @see {@link Column#isValuePresentOrOptional(ValueSet)}
	 */
	protected boolean isFilled(ColumnSet columnSet) throws InvalidColumnException
	{
		return isFilled(columnSet, ColumnSet.SKIP_NONE, false); // don't recurse by default
	}
	
	/**
	 * Checks, optionally recursive, whether all non-optional columns from the given columnSet have been assigned a non-{@code null} value in this ValueSet.
	 * The given ColumnSet must be this ValueSet's ColumnSet or a subset of it.
	 * This method was added for the purpose of checking whether primary keys (which are a subset of a schema's columns) have been set.
	 * 
	 * @param columnSet (subset of) the record's schema
	 * @param skipColumns a set of columns to ignore in this check
	 * @param recurse whether or not to recursively check whether the non-optional subcolumns of composite columns also have a non-{@code null} value
	 * @return whether all of the non-optional columns are filled
	 * @throws InvalidColumnException when the given ColumnSet contains a column(s) which is not part of the ValueSet's ColumnSet
	 * 
	 * @see {@link Column#isValuePresentOrOptional(ValueSet,boolean)}
	 */
	protected boolean isFilled(ColumnSet columnSet, Set<? extends Column<?>> skipColumns, final boolean recurse) throws InvalidColumnException
	{
		for(Column<?> col : columnSet.getColumns(false, skipColumns))
			if(!col.isValuePresentOrOptional(this, recurse))
				return false; // null value in non-optional column	
		return true;
	}
	
	/**
	 * Checks whether this ValueSet has a {@code null} value for each of the (non-virtual) columns in its ColumnSet.
	 * 
	 * @return whether there is a {@code null} value in each column
	 * 
	 * @see {@link Column#isValuePresent(ValueSet, boolean)}
	 */
	public boolean isEmpty()
	{
		return isEmpty(false); // don't recurse by default
	}
	
	/**
	 * Checks whether this ValueSet has a {@code null} value for each of the (non-virtual and non-skipped) columns in its ColumnSet.
	 * 
	 * @param skipColumns a set of columns to ignore in this check
	 * @return whether there is a {@code null} value in each column
	 * 
	 * @see {@link Column#isValuePresent(ValueSet, boolean)}
	 */
	public boolean isEmpty(Set<? extends Column<?>> skipColumns)
	{
		return isEmpty(skipColumns, false); // don't recurse by default
	}
	
	/**
	 * Checks, optionally recursive, whether this ValueSet has a {@code null} value for each of the (non-virtual) columns in its ColumnSet.
	 * 
	 * @param recurse whether or not to recursively check whether the subcolumns of composite columns also have all {@code null} values
	 * @return whether there is a {@code null} value in each column
	 * 
	 * @see {@link Column#isValuePresent(ValueSet, boolean)}
	 */
	public boolean isEmpty(final boolean recurse)
	{
		return isEmpty(ColumnSet.SKIP_NONE, recurse);
	}
	
	/**
	 * Checks, optionally recursive, whether this ValueSet has a {@code null} value for each of the (non-virtual and non-skipped) columns in its ColumnSet.
	 * 
	 * @param skipColumns a set of columns to ignore in this check
	 * @param recurse whether or not to recursively check whether the subcolumns of composite columns also have all {@code null} values
	 * @return whether there is a {@code null} value in each column
	 * 
	 * @see {@link Column#isValuePresent(ValueSet, boolean)}
	 */
	protected boolean isEmpty(Set<? extends Column<?>> skipColumns, boolean recurse)
	{
		for(Column<?> col : columnSet.getColumns(false, skipColumns))
			if(col.isValuePresent(this, recurse))
				return false;
		return true;
	}
	
	/**
	 * Check whether this ValueSet holds the default value for each of the (non-virtual) columns in its ColumnSet.
	 * 
	 * @return whether there is a {@code null} value in each column
	 * 
	 * @see {@link Column#isValueDefault(ValueSet)}
	 */
	protected boolean isDefault()
	{
		return isDefault(ColumnSet.SKIP_NONE);
	}
	
	/**
	 * Check whether this ValueSet holds the default value for each of the (non-virtual and non-skipped) columns in its ColumnSet.
	 * 
	 * @param skipColumns a set of columns to ignore in this check
	 * @return whether there is a {@code null} value in each column
	 * 
	 * @see {@link Column#isValueDefault(ValueSet)}
	 */
	protected boolean isDefault(Set<? extends Column<?>> skipColumns)
	{
		for(Column<?> col : columnSet.getColumns(false, skipColumns))
			if(!col.isValueDefault(this))
				return false;
		return true;
	}
	
	/**
	 * Method which resets the values of (non-virtual) columns which are currently empty (i.e. containing a {@code null} value)
	 * to their {@link #defaultValue} (assumed be be non-{@code null}), optionally only for required (i.e. non-optional) columns.
	 * 
	 * @param onlyIfRequired whether to only reset required columns ((@code true}) or also optional ones ({@code false})
	 * 
	 * @see {@link Column#resetValue(ValueSet)}
	 * @see {@link Column#resetIfEmpty(ValueSet, boolean)}
	 */
	public void resetEmptyColumns(final boolean onlyIfRequired)
	{
		resetEmptyColumns(onlyIfRequired, false); // don't recurse by default
	}
	
	/**
	 * Method which resets the values of (non-virtual and non-skipped) columns which are currently empty (i.e. containing a {@code null}
	 * value) to their {@link #defaultValue} (assumed be be non-{@code null}), optionally only for required (i.e. non-optional) columns.
	 * 
	 * @param skipColumns a set of columns to ignore in this operation
	 * @param onlyIfRequired whether to only reset required columns ((@code true}) or also optional ones ({@code false})
	 * 
	 * @see {@link Column#resetValue(ValueSet)}
	 * @see {@link Column#resetIfEmpty(ValueSet, boolean)}
	 */
	public void resetEmptyColumns(Set<? extends Column<?>> skipColumns, boolean onlyIfRequired)
	{
		resetEmptyColumns(skipColumns, onlyIfRequired, false); // don't recurse by default
	}
	
	/**
	 * Method which resets the values of (non-virtual) (sub)columns which are currently empty (i.e. containing a {@code null} value)
	 * to their {@link #defaultValue} (assumed be be non-{@code null}), optionally only for required (i.e. non-optional) columns.
	 *
	 * @param onlyIfRequired whether to only reset required columns ((@code true}) or also optional ones ({@code false})
	 * @param recurse whether or not to apply the operation recursively to all subColumns (only relevant if this is a composite Column)
	 * 
	 * @see {@link Column#resetValue(ValueSet)}
	 * @see {@link Column#resetIfEmpty(ValueSet, boolean, boolean)}
	 */
	public void resetEmptyColumns(boolean onlyIfRequired, boolean recurse)
	{
		resetEmptyColumns(ColumnSet.SKIP_NONE, onlyIfRequired, recurse);
	}
	
	/**
	 * Method which resets the values of (non-virtual and non-skipped) (sub)columns which are currently empty (i.e. containing a {@code null}
	 * value) to their {@link #defaultValue} (assumed be be non-{@code null}), optionally only for required (i.e. non-optional) columns.
	 * 
	 * @param skipColumns a set of columns to ignore in this operation
	 * @param onlyIfRequired whether to only reset required columns ((@code true}) or also optional ones ({@code false})
	 * @param recurse whether or not to apply the operation recursively to all subColumns (only relevant if this is a composite Column)
	 * 
	 * @see {@link Column#resetValue(ValueSet)}
	 * @see {@link Column#resetIfEmpty(ValueSet, boolean, boolean)}
	 */
	public void resetEmptyColumns(Set<? extends Column<?>> skipColumns, boolean onlyIfRequired, boolean recurse)
	{
		for(Column<?> col : columnSet.getColumns(false, skipColumns))
			col.resetIfEmpty(this, onlyIfRequired, recurse);
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
	 * Checks whether this ValueSet has a valid value for each of the (non-virtual and non-skipped) columns in its ColumnSet.
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
	 * Checks, optionally recursive, whether this ValueSet has a valid value for each of the (non-virtual) columns in its ColumnSet.
	 * Note that is it should not actually be possible to have stored invalid values in the ValueSet.
	 * 
	 * @param recurse whether or not to recursively check whether the subcolumns of composite columns also have valid values
	 * @return whether there is a valid value for each column
	 * 
	 * @see {@link Column#isValueValid(ValueSet,boolean)}
	 */
	public boolean isValid(final boolean recurse)
	{
		return isValid(ColumnSet.SKIP_NONE, recurse);
	}
	
	/**
	 * Checks, optionally recursive, whether this ValueSet has a valid value for each of the (non-virtual and non-skipped) columns in its ColumnSet.
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
		for(Column<?> col : columnSet.getColumns(false, skipColumns))
			if(!col.isValueValid(this, recurse))
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
	 * Serialise the ValueSet to a String, excluding virtual columns.
	 * 
	 * @return
	 * @see #serialise(boolean, Set)
	 */
	public String serialise()
	{
		return serialise(false, ColumnSet.SKIP_NONE);
	}
	
	/**
	 * Serialise the ValueSet to a String.
	 * 
	 * @param includeVirtual
	 * @param skipColumns
	 * @return
	 */
	public String serialise(boolean includeVirtual, Set<? extends Column<?>> skipColumns)
	{
		StringBuilder bldr = new StringBuilder();
		boolean first = true;
		for(Column<?> col : columnSet.getColumns(includeVirtual, skipColumns))
		{
			// Separator:
			if(first)
				first = false;
			else
				bldr.append(SERIALISATION_SEPARATOR);
			// Value:
			String valueString = col.retrieveValueAsString(this, true); // will return empty String for null values
			// 	If the column does not apply it's own serialisation delimiting then
			//	 we may have to wrap the valueString in the default serialisation delimiters:
			if(!col.isApplyingSerialisationDelimiting())
				valueString = StringUtils.escapeByDoublingAndWrapping(valueString, DEFAULT_SERIALISATION_DELIMITER, /*don't force:*/ false, SERIALISATION_SEPARATOR);
			bldr.append(valueString == null ? "" /*just in case*/ : valueString);
		}
		return bldr.toString();
	}
	
	/**
	 * Deserialise the values of a ValueSet from a String (expected to be in the new format), not expecting virtual columns.
	 * 
	 * @param serialisedValueSet should not be {@code null}
	 * @throws Exception
	 * @return this ValueSet
	 * @see #parse(String, boolean, Set)
	 */
	public ValueSet<CS> parse(String serialisedValueSet) throws Exception
	{
		return parse(serialisedValueSet, false, ColumnSet.SKIP_NONE);
	}
	
	/**
	 * Deserialise the values of a ValueSet from a String.
	 * 
	 * @param serialisedValueSet should not be {@code null}
	 * @param includeVirtual whether or not to expect values for the virtual columns
	 * @param skipColumns a set of columns not to expect values for
	 * @return this ValueSet
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @see {@link #serialise(boolean, Set)}
	 */
	public ValueSet<CS> parse(String serialisedValueSet, boolean includeVirtual, Set<? extends Column<?>> skipColumns) throws ParseException, IllegalArgumentException, NullPointerException
	{
		if(serialisedValueSet == null)
			throw new IllegalArgumentException("Cannot parse null String, it represents a null ValueSet object");

		// Get columns included in serialisedValueSet:
		List<Column<?>> expectedColumns = columnSet.getColumns(includeVirtual, skipColumns);

		// Add trailing separator (this simplifies the code below):
		serialisedValueSet += SERIALISATION_SEPARATOR;
		
		// Parse serialisedValueSet column by column (splitting the string by looking for separators outside of delimited values):
		Iterator<Column<?>> colIter = expectedColumns.iterator();
		int valueCount = 0;
		Column<?> col = null;
		char colDelimiter = DEFAULT_SERIALISATION_DELIMITER;
		int colDelimiterCount = 0;
		StringBuilder valueStringBldr = new StringBuilder();
		for(char c : serialisedValueSet.toCharArray())
		{
			// Get column:
			if(col == null)
			{
				if(colIter.hasNext())
				{
					col = colIter.next();
					colDelimiter = col.getSerialisationDelimiter() != null ? col.getSerialisationDelimiter() : DEFAULT_SERIALISATION_DELIMITER;
					colDelimiterCount = 0; // !!!
				}
				else
				{
					valueCount++; // there is at least 1 more value to parse
					break; // stop parsing...
				}
			}
			// Treat character:
			if(c == colDelimiter)
			{	// count the number of colDelimiters we've passed
				colDelimiterCount++;
				// Append char:
				valueStringBldr.append(c); // !!!
			}
			else if(c == SERIALISATION_SEPARATOR && colDelimiterCount % 2 == 0)
			{	// if delimiterCount is even this means we are not *inside* a value and this is an actual serialisation separator
				// Parse value, unless this is a virtual column:
				if(	(!includeVirtual || !(col instanceof VirtualColumn)))	// ignore virtual column values as they never store their own value
				{
					String valueString = valueStringBldr.toString();
					
					// If the column does not apply it's own serialisation delimiting then
					//	it could be that valueString is wrapped using the default serialisation delimiters:
					if(!col.isApplyingSerialisationDelimiting())
						valueString = StringUtils.deescapeByDoublingAndWrapping(valueString, DEFAULT_SERIALISATION_DELIMITER);
					
					// Parse & store:
					col.storeString(this, valueString); // if the column applies it's own delimiters these will be removed/deescaped; validation will be performed
				}
				
				// We are done with this column:
				valueCount++;
				col = null;
				valueStringBldr.setLength(0); // reset valueString builder!
			}
			else
			{	
				// Append char:
				valueStringBldr.append(c); // !!!
			}
		}
		
		// Check number of values:
		if(valueCount > expectedColumns.size()) // more values than expected...
			throw new IllegalArgumentException("Expecting values for " + expectedColumns.size() + " columns, given string contains at least " + valueCount + "!");
		if(colIter.hasNext()) // less values than expected...
			throw new IllegalArgumentException("Expecting values for " + expectedColumns.size() + " columns, given string contains only " + valueCount + "!");
		
		// Return self:
		return this;
	}
	
	/**
	 * Serialise the ValueSet to a byte array, excluding virtual columns.
	 * 
	 * @param lossless if {@code true} all values are to be losslessly encoded; if {@code false} the values of columns which {@link #canBeLossy()} are to be lossyly encoded, and the values of the others losslessly.
	 * @return a byte[] representation of the values of this ValueSet
	 * @throws IOException
	 */
	public byte[] toBytes(boolean lossless) throws IOException
	{
		BitOutputStream out = null;
		try
		{
			// Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitWrapOutputStream(rawOut);
				
			// Write record:
			this.writeToBitStream(out, lossless);
			
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
	 * @param lossless if {@code true} all values are to be losslessly encoded; if {@code false} the values of columns which {@link #canBeLossy()} are to be lossyly encoded, and the values of the others losslessly.
	 * @throws IOException
	 */
	public void writeToBitStream(BitOutputStream bitStream, boolean lossless) throws IOException
	{
		writeToBitStream(bitStream, false, ColumnSet.SKIP_NONE, lossless);
	}
	
	/**
	 * Write ValueSet values, possibly including those of virtual columns and except the given skipped ones, to the given bitStream.
	 * 
	 * @param bitStream
	 * @param includeVirtual whether or not to include the values corresponding to virtual columns
	 * @param skipColumns columns *not* to include the values of
	 * @param lossless if {@code true} all values are to be losslessly encoded; if {@code false} the values of columns which {@link #canBeLossy()} are to be lossyly encoded, and the values of the others losslessly.
	 * @throws IOException
	 */
	public void writeToBitStream(BitOutputStream bitStream, boolean includeVirtual, Set<? extends Column<?>> skipColumns, boolean lossless) throws IOException
	{
		writeColumnsToBitStream(bitStream, columnSet.getColumns(includeVirtual, skipColumns), lossless);
	}
	
	/**
	 * Write ValueSet values of the given columns (in given order) to the given bitStream.
	 * 
	 * @param bitStream
	 * @param columns columns to include the values of
	 * @param lossless if {@code true} all values are to be losslessly encoded; if {@code false} the values of columns which {@link #canBeLossy()} are to be lossyly encoded, and the values of the others losslessly.
	 * @throws IOException
	 */
	public void writeColumnsToBitStream(BitOutputStream bitStream, List<? extends Column<?>> columns, boolean lossless) throws IOException
	{
		try
		{	// Write fields:
			for(Column<?> c : columns)
				c.retrieveAndWriteValue(this, bitStream, lossless);
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
	 * @param lossless if {@code true} all values are expected to be losslessly encoded; if {@code false} the values of columns which {@link #canBeLossy()} are expected to be lossyly encoded, and the values of the others losslessly.
	 * @return the record itself
	 * @throws IOException
	 */
	public ValueSet<CS> fromBytes(byte[] bytes, boolean lossless) throws IOException
	{
		BitInputStream in = null;
		try
		{
			// Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(bytes);
			in = new BitWrapInputStream(rawIn);
				
			// Read record:
			this.readFromBitStream(in, lossless);
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
	 * @param lossless if {@code true} all values are expected to be losslessly encoded; if {@code false} the values of columns which {@link #canBeLossy()} are expected to be lossyly encoded, and the values of the others losslessly.
	 * @throws IOException
	 */
	public void readFromBitStream(BitInputStream bitStream, boolean lossless) throws IOException
	{
		readFromBitStream(bitStream, false, ColumnSet.SKIP_NONE, lossless);
	}
	
	/**
	 * Read ValueSet values, possibly including those of virtual columns and except the given skipped ones, from the given bitStream.
	 * 
	 * @param bitStream
	 * @param includeVirtual whether or not to expect the values corresponding to virtual columns, if {@code true} these values will be read from the stream but not stored (i.e. they will effectively be skipped)
	 * @param skipColumns columns *not* to expect the values of
	 * @param lossless if {@code true} all values are expected to be losslessly encoded; if {@code false} the values of columns which {@link #canBeLossy()} are expected to be lossyly encoded, and the values of the others losslessly.
	 * @throws IOException
	 */
	public void readFromBitStream(BitInputStream bitStream, boolean includeVirtual, Set<? extends Column<?>> skipColumns, boolean lossless) throws IOException
	{
		readColumnsFromBitStream(bitStream, columnSet.getColumns(includeVirtual, skipColumns), lossless);
	}
	
	/**
	 * Read ValueSet values of the given columns (in given order) from the given bitStream.
	 * 
	 * @param bitStream
	 * @param columns columns to expect the values of
	 * @param lossless if {@code true} all values are expected to be losslessly encoded; if {@code false} the values of columns which {@link #canBeLossy()} are expected to be lossyly encoded, and the values of the others losslessly.
	 * @throws IOException
	 */
	public void readColumnsFromBitStream(BitInputStream bitStream, List<? extends Column<?>> columns, boolean lossless) throws IOException
	{
		try
		{	// Read fields:
			for(Column<?> c : columns)
			{
				if(c instanceof VirtualColumn)
					c.readValue(bitStream, lossless); // read but don't store values of virtual columns (i.e. we skip them in the stream)
				else
					c.readAndStoreValue(this, bitStream, lossless);
			}
		}
		catch(Exception e)
		{
			throw new IOException("Error on attempting to read record. Read so far: " + this.toString(), e);
		}
	}
	
	/**
	 * Gets the size of this ValueSet in number of bits, when written to binary representation.
	 * 
	 * @param lossless whether to use lossless ({@code true}) or lossy ({@code false}) encoding
	 * @return
	 */
	public int getSize(boolean includeVirtual, Set<? extends Column<?>> skipColumns, boolean lossless)
	{
		BitOutputStream out = null;
		try
		{
			out = new BitWrapOutputStream(new ByteArrayOutputStream());
			this.writeToBitStream(out, includeVirtual, skipColumns, lossless);
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
	
	/**
	 * <p>
	 * Checks whether this ValueSet is *likely* to be the result of lossy storage/transmission.</p>
	 * <p>
	 * This is done by comparing each current value to its lossyly encoded version, if all are
	 * equal then the ValueSet is assumed to be the result of lossy storage/transmission (because
	 * there is no further information loss when re-encoding now). If at least one value is different
	 * from its lossyly encoded version than the ValueSet is assumed to still be in a lossless state.</p>
	 * <p>
	 * This method not 100% foolproof because it relies on assumptions which may not always hold true:<br/>
	 * First there is the assumption that lossless values are always different from their lossy version.
	 * This is not always the case, for example a timestamp like 2015-10-21T11:29:54.000+02:00 will stay
	 * the same in lossy form because the number of ms is 0 already. Were this to happen for all columns
	 * which can be lossy then the whole ValueSet will be considered lossy even if it is still lossless.
	 * This issue is side-stepped if the ColumnSet contains the {@link LosslessFlagColumn}, because for
	 * this column a lossless value will never equal the lossy version. In the presence of this column
	 * this method thus becomes a lot more reliable, even though it is not directly used here (unlike
	 * in the {@link Record#isLossy()} and {@link Record#isLossless()} overrides).<br/>
	 * Second there is the assumption that once a value has been encoded lossyly it will not change
	 * again if it is encoded lossyly again later (in other words, there is no further information loss).
	 * This is true for all currently existing columns which support lossy encoding.</p>
	 * 
	 * @return
	 */
	public boolean isLossy()
	{
		if(!columnSet.canBeLossy())
			return false;
		for(Column<?> c : columnSet.getColumns(false))
			if(!Objects.deepEquals(c.retrieveValue(this), c.retrieveValueAsLossy(this))) // do *not* call c.retrieveValue(this, true) instead of c.retrieveAsLossy(this) (it would cause an endless loop)
				return false; // at least 1 value is different from its lossy version --> this ValueSet is considered lossless
		return true;
	}
	
	/**
	 * Checks whether this ValueSet is *likely* to still be in a lossless state.
	 * 
	 * @return
	 * @see #isLossy()
	 */
	public boolean isLossless()
	{
		return !isLossy();
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
	 * @param asLossyEncoded whether or not to compare values as if they've been "lossyly" written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean equals(Object obj, boolean checkColumnSet, boolean asLossyEncoded)
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
			return hasEqualValues(other, asLossyEncoded);
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
		return hasEqualValues(other, ColumnSet.SKIP_NONE);
	}
	
	/**
	 * Compare the values of this ValueSet with those of another.
	 * If {@code asStoredBinary} is {@code true} the ValueSets must be of the same schema, otherwise an exception will be thrown.
	 * 
	 * @param other another ValueSet
	 * @param asLossyEncoded whether or not to compare values as if they've been "lossyly" written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean hasEqualValues(ValueSet<?> other, boolean asLossyEncoded)
	{
		return hasEqualValues(other, ColumnSet.SKIP_NONE, asLossyEncoded);
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
	 * If {@code asStoredBinary} is {@code true} the records must be of the same schema, otherwise an exception will be thrown.
	 * 
	 * @param other another ValueSet
	 * @param skipColumns ignore these columns
	 * @param asLossyEncoded whether or not to compare values as if they've been "lossyly" written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean hasEqualValues(ValueSet<?> other, Set<? extends Column<?>> skipColumns, boolean asLossyEncoded)
	{
		return other == null ?	false :
								(!skipColumns.isEmpty() || asLossyEncoded ?
									hasEqualValuesForColumns(other, this.columnSet.getColumns(false, skipColumns), asLossyEncoded) :
									this == other || Arrays.deepEquals(this.values, other.values));
	}
	
	/**
	 * Compare the values of this ValueSet with those of another, across the given list of columns.
	 * This and the other record as assumed to have schemata that are the same or at least each share the given columns (or equivalents).
	 * 
	 * @param other another ValueSet
	 * @param columns the columns that will be checked, unless they appear in skipColumns
	 * @return
	 */
	public boolean hasEqualValuesForColumns(ValueSet<?> other, List<? extends Column<?>> columns)
	{
		return hasEqualValuesForColumns(other, columns, false);
	}
	
	/**
	 * Compare the values of this ValueSet with those of another, across the given collection of columns.
	 * This and the other record as assumed to have schemata that are the same or at least each share the given columns (or equivalents).
	 * 
	 * @param other another ValueSet
	 * @param columns the columns that will be checked, unless they appear in skipColumns
	 * @param asLossyEncoded whether or not to compare values as if they've been "lossyly" written/read to/from a bitstream (meaning some elements may have been dropped or precision may have been reduced)
	 * @return
	 */
	public boolean hasEqualValuesForColumns(ValueSet<?> other, Collection<? extends Column<?>> columns, boolean asLossyEncoded)
	{
		if(other == null)
			return false;
		if(this != other)
			for(Column<?> c : columns)
				if(	!Objects.deepEquals(c.retrieveValue(this, asLossyEncoded),
										c.retrieveValue(other, asLossyEncoded)))
					return false;
		return true;
	}
	
}
