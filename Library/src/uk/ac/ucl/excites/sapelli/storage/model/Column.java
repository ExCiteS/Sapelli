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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.StreamHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLNameEncoder;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLUtils;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * <p>
 * Abstract class representing database schema/table column of generic type {@code T}.</p>
 * <p>
 * Note that although this class implements {@link Comparator<ValueSet<?>>} the different
 * {@code compare} methods are only guaranteed to group identical values (or the ValueSets
 * that contain them) together, not to also rank them in a meaningful, "logical" or domain-
 * specific order (see {@link #compareNonNullValues(Object, Object)}).</p>
 *
 * @param <T> the content type
 * 
 * @author mstevens
 */
public abstract class Column<T> implements Serializable, Comparator<ValueSet<?>>
{

	// STATICS-------------------------------------------------------
	private static final long serialVersionUID = 2L;

	static public final char ILLEGAL_NAME_CHAR_REPLACEMENT = '_';

	/**
	 * @param name the String to be sanitised for use as a Column name
	 * @return a version of the given name which is acceptable for use as a Column name
	 */
	static public String SanitiseName(String name)
	{
		// Check for null & empty String:
		if(name == null || name.isEmpty())
			throw new IllegalArgumentException("Please provide a non-null, non-empty name to be sanitised");
		// Perform basic sanitation:
		//	Detect & replace the most common illegal characters with a simple underscore; except those that come last, which are just removed
		StringBuilder bldr = new StringBuilder();
		int prevNeedsReplace = 0;
		for(char c : name.toCharArray())
			switch(c)
			{
				// Not allowed anywhere:
				case '.'		:	/* not allowed at start of XML names, but nowhere in our Column names because it is the ValueSetColumn.QUALIFIED_NAME_SEPARATOR */
				case ','		:	/* not allowed anywhere in XML names. Moreover it is one of our supported CSV separators. */
				case '?'		:	/* not allowed anywhere in XML names. */
				case '!'		:	/* not allowed anywhere in XML names. */
				case ':'		:	/* is allowed anywhere in XML names but we take it out anyway to avoid some XML tools recognising it as an XML namespace separator */
				case ';'		:	/* not allowed anywhere in XML names. Moreover it is one of our supported CSV separators. */
				case '\''		:	/* not allowed anywhere in XML names. */
				case '"'		:	/* not allowed anywhere in XML names. */
				case '/'		:	/* not allowed anywhere in XML names. */
				case '\\'		:	/* not allowed anywhere in XML names. */
				case '@'		:	/* not allowed anywhere in XML names. */
				case '('		:	/* not allowed anywhere in XML names. */
				case ')'		:	/* not allowed anywhere in XML names. */
				case '['		:	/* not allowed anywhere in XML names. */
				case ']'		:	/* not allowed anywhere in XML names. */
				case '{'		:	/* not allowed anywhere in XML names. */
				case '}'		:	/* not allowed anywhere in XML names. */
				case '&'		:	/* not allowed anywhere in XML names. */
				case '%'		:	/* not allowed anywhere in XML names. */
				case '$'		:	/* not allowed anywhere in XML names. */
				case '\u00A3'	:	/* (Pound sign) not allowed anywhere in XML names. */
				case '+'		:	/* not allowed anywhere in XML names. */
				case '*'		:	/* not allowed anywhere in XML names. */
				case '#'		:	/* not allowed anywhere in XML names. */
				case '|'		:	/* not allowed anywhere in XML names. */
				case '~'		:	/* not allowed anywhere in XML names. */
				case ' '		:	/* not allowed anywhere in XML names. */
				case '\t'		:	/* not allowed anywhere in XML names. Moreover it is one of our supported CSV separators. */
				case '\r'		:	/* not allowed anywhere in XML names. */
				case '\n'		:	/* not allowed anywhere in XML names. */
				case '='		:	/* is allowed anywhere in XML names, but we used in special header fields of our CSV format which shouldn't be confused for column headers. */
					prevNeedsReplace++;
					break;
				// Not allowed at start, OK elsewhere:
				case '-'	:	/* not allowed at start of XML names.*/
					if(bldr.length() == 0)
					{
						prevNeedsReplace++;
						break;
					}
				default		:
					// Insert pending replacements:
					for(int i = 0; i  < prevNeedsReplace; i++)
						bldr.append(ILLEGAL_NAME_CHAR_REPLACEMENT);
					prevNeedsReplace = 0;
					// Insert unchanged char:
					bldr.append(c);
			}
		name = bldr.toString();

		// Perform further XML-specific sanitation if needed:
		//	Check if the result is (now) a valid XML name and if not, make it one (using more complex escape mechanism)
		if(!XMLUtils.isValidName(name, XMLRecordsExporter.USES_XML_VERSION_11))
			return XMLNameEncoder.encode(name);
		else
			return name;
	}

	/**
	 * @param name
	 * @return whether or not the given name is valid Column name
	 */
	static public boolean IsValidName(String name)
	{
		return name != null && !name.isEmpty() && name.equals(SanitiseName(name));
	}

	// DYNAMICS------------------------------------------------------
	public final String name;
	public final boolean optional;
	private List<VirtualColumn<?, T>> virtualVersions;
	public final T defaultValue;

	/**
	 * @param name
	 * @param optional
	 * @param defaultValue
	 * @throws IllegalArgumentException in case the defaultValue is invalid
	 */
	public Column(String name, boolean optional, T defaultValue) throws IllegalArgumentException
	{
		if(!IsValidName(name))
			throw new IllegalArgumentException("Invalid column name (" + name + "), please use the static Column#SanitiseName method.");
		this.name = name;
		this.optional = optional;
		this.defaultValue = defaultValue;
		if(defaultValue != null)
			validate(defaultValue);
	}

	/**
	 * @return a copy of this Column
	 */
	public abstract Column<T> copy();

	/**
	 * @param valueSet {@link ValueSet} to store the parsed value in, should not be {@code null}
	 * @param valueString may be {@code null} or empty {@code String} but both cases treated as representing a {@code null} value
	 * @throws ParseException
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is, or when the parse value is invalid
	 * @throws NullPointerException if the parsed value is {@code null} on an non-optional column, or if the valueSet is {@code null}
	 */
	public void parseAndStoreValue(ValueSet<?> valueSet, String valueString) throws ParseException, IllegalArgumentException, NullPointerException
	{
		storeValue(valueSet, stringToValue(valueString));
	}
	
	/**
	 * @param valueString may be {@code null} or empty {@code String} but both cases treated as representing a {@code null} value
	 * @return the corresponding value of type {@code <T>}, or {@code null} if the given valueString was {@code null} or empty {@code String}
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	public T stringToValue(String valueString) throws ParseException, IllegalArgumentException, NullPointerException
	{
		if(valueString == null || valueString.isEmpty()) // empty String is treated as null!
			return null;
		else
			return parse(valueString);
	}

	/**
	 * @param valueString the {@link String} to parse, should be neither {@code null} nor empty {@code String}!
	 * @return the parsed value as type {@code <T>}
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	public abstract T parse(String valueString) throws ParseException, IllegalArgumentException, NullPointerException;

	/**
	 * Stores the given Object value in this column on the given record.
	 *
	 * @param valueSet {@link ValueSet} to store the value in, should not be {@code null}
	 * @param valueObject value to store, given as an {@link Object} (may be converted first), is allowed to be {@code null} only if column is optional
	 * @throws IllegalArgumentException in case of a columnSet mismatch or invalid value
	 * @throws NullPointerException if value is null on an non-optional column
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type {@code <T>}
	 * @see #convert(Object)
	 */
	public void storeObject(ValueSet<?> valueSet, Object valueObject) throws IllegalArgumentException, NullPointerException, ClassCastException
	{
		storeValue(valueSet, convert(valueObject));
	}
	
	/**
	 * Retrieves previously stored value for this column at a given {@code from} ValueSet and 
	 * stores is in this column of the given {@code to} ValueSet. Performs optionality check and validation.
	 * 
	 * @param from the {@link ValueSet} to retrieve the value from, should not be {@code null}
	 * @param to the valueSet in which to store the value, may not be {@code null}
	 * @throws IllegalArgumentException when this column is not part of one of the ValueSets' {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws NullPointerException if value is {@code null} on an non-optional column, or if one of the ValueSets is {@code null}
	 */
	public void copyValue(ValueSet<?> from, ValueSet<?> to)
	{
		storeValue(to, retrieveValue(from));
	}
	
	/**
	 * Stores the given {@code <T>} value in this column on the given valueSet. Performs optionality check and validation.
	 *
	 * @param valueSet the valueSet in which to store the value, may not be {@code null}
	 * @param value the value to store, may be {@code null} if column is optional
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is, or when the given value is invalid
	 * @throws NullPointerException if value is {@code null} on an non-optional column, or if the valueSet is {@code null}
	 */
	public void storeValue(ValueSet<?> valueSet, T value) throws IllegalArgumentException, NullPointerException, UnsupportedOperationException
	{
		// Check:
		if(value == null)
		{
			if(!optional)
				throw new NullPointerException("Cannot set null value for non-optional column \"" + getName() + "\"!");
		}
		else
			validate(value); // throws IllegalArgumentException if invalid
		// Store:
		storeValueUnchecked(valueSet, value);
	}
	
	/**
	 * @param valueSet the valueSet in which to store the value, may not be {@code null}
	 * @param value the value to store, may be {@code null} (which will wipe earlier non-{@code null} values)
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws NullPointerException if the valueSet is {@code null}
	 */
	private final void storeValueUnchecked(ValueSet<?> valueSet, T value) throws IllegalArgumentException, NullPointerException
	{
		valueSet.setValue(this, value); // also store null (to overwrite earlier non-null value)
	}
	
	/**
	 * (Re-)sets the value of this column in the given valueSet to {@code null}, even if the column is non-optional(!).
	 * Use with care!
	 * 
	 * @param valueSet the valueSet in which to clear the value, may not be {@code null}
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws NullPointerException if the valueSet is {@code null}
	 */
	public final void clearValue(ValueSet<?> valueSet) throws IllegalArgumentException, NullPointerException
	{
		storeValueUnchecked(valueSet, null);
	}
	
	/**
	 * Resets the value of this column in the given valueSet to the {@link #defaultValue} (usually {@code null}), even if the column is non-optional(!).
	 * Use with care!
	 * 
	 * @param valueSet
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	public final void resetValue(ValueSet<?> valueSet) throws IllegalArgumentException, NullPointerException
	{
		storeValueUnchecked(valueSet, defaultValue);
	}

	/**
	 * Retrieves previously stored value for this column at a given valueSet and casts it to the relevant native type (T).
	 *
	 * @param valueSet the {@link ValueSet} to retrieve the value from, should not be {@code null}
	 * @return stored value (may be {@code null})
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 */
	@SuppressWarnings("unchecked")
	public <VS extends ValueSet<CS>, CS extends ColumnSet> T retrieveValue(VS valueSet) throws IllegalArgumentException
	{
		return (T) valueSet.getValue(this);
	}

	/**
	 * Checks whether a non-{code null} value for this column is set in the given valueSet.
	 *
	 * @param valueSet should not be {@code null}
	 * @return whether or not a non-{@code null} value is set
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	public final boolean isValuePresent(ValueSet<?> valueSet) throws IllegalArgumentException
	{
		return isValuePresent(valueSet, false); // don't recurse by default
	}
	
	/**
	 * Checks, possibly recursively, whether a non-{code null} value for this column is set in the given valueSet.
	 *
	 * @param valueSet should not be {@code null}
	 * @param recurse whether or not to check recursively if all subColumns also have a non-{@code null} value (only relevant if this is a composite Column)
	 * @return whether or not a non-{@code null} value is set
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	public boolean isValuePresent(ValueSet<?> valueSet, boolean recurse) throws IllegalArgumentException
	{
		return retrieveValue(valueSet) != null;
	}
	
	/**
	 * Checks whether this column either has a non-{code null} value in the given valueSet or is optional.
	 * 
	 * @param valueSet should not be {@code null}
	 * @return whether a non-{@code null} value is set or the column is optional 
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	public final boolean isValuePresentOrOptional(ValueSet<?> valueSet) throws IllegalArgumentException
	{
		return isValuePresentOrOptional(valueSet, false); // don't recurse by default
	}
	
	/**
	 * Checks, possibly recursively, whether this column either has a non-{code null} value in the given valueSet or is optional.
	 * 
	 * @param valueSet should not be {@code null}
	 * @param recurse whether or not to check recursively if all subColumns also have a non-{@code null} value or are optional (only relevant if this is a composite Column)
	 * @return whether a non-{@code null} value is set or the column is optional 
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	public boolean isValuePresentOrOptional(ValueSet<?> valueSet, boolean recurse) throws IllegalArgumentException
	{
		if(isValuePresent(valueSet))
			return true;
		else
			return optional;
	}
	
	/**
	 * Checks whether this column has a valid value in the given valueSet.
	 * A {@code null} value is valid only if it the column is optional. A non-{@code null} value is valid if it passes the {@link #validate(Object)} tests.
	 * 
	 * @param valueSet should not be {@code null}
	 * @return whether the value currently contained by the valueSet for this column is valid (note that a {@code null} value is valid if the column is optional)
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	public final boolean isValueValid(ValueSet<?> valueSet) throws IllegalArgumentException
	{
		return isValueValid(valueSet, false); // don't recurse by default
	}
	
	/**
	 * Checks, possibly recursively, whether this column has a valid value in the given valueSet.
	 * A {@code null} value is valid only if it the column is optional. A non-{@code null} value is valid if it passes the {@link #validate(Object)} tests.
	 * 
	 * @param valueSet should not be {@code null}
	 * @param recurse whether or not to check recursively if all subColumns also valid values (only relevant if this is a composite Column)
	 * @return whether the value currently contained by the valueSet for this column is valid (note that a {@code null} value is valid if the column is optional)
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	public boolean isValueValid(ValueSet<?> valueSet, boolean recurse) throws IllegalArgumentException
	{
		return isValidValue(retrieveValue(valueSet));
	}

	/**
	 * @param valueSet should not be {@code null}
	 * @return a {@link String} representation of the value retrieved from the valueSet for this Column, or {@code null} if the valueSet did not contain a value for this Column
	 */
	public String retrieveValueAsString(ValueSet<?> valueSet)
	{
		return valueToString(retrieveValue(valueSet));
	}
	
	/**
	 * @param value may be {@code null}, in which case {@code null} is returned
	 * @return a {@link String} representation of the value, or {@code null} if the value was {@code null}
	 */
	public String valueToString(T value)
	{
		if(value != null)
			return toString(value);
		else
			return null;
	}

	/**
	 * @param valueObject may be {@code null}, in which case {@code null} is returned
	 * @return a String representation of the valueObject, or {@code null} if the valueObject was {@code null}
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type {@code <T>}
	 * @see #convert(Object)
	 */
	public String objectToString(Object valueObject) throws ClassCastException
	{
		return valueToString(convert(valueObject));
	}
	
	/**
	 * Default implementation only performs an unchecked cast.
	 * To be overridden by subclasses which need to perform additional conversion in order to accept a wider range of value types.
	 * 
	 * @param valueObject as {@link Object}, or {@code null}
	 * @return converted value of type {@code <T>}, or {@code null} if given valueObject was {@code null}
	 * @throws ClassCastException
	 */
	@SuppressWarnings("unchecked")
	public T convert(Object valueObject) throws ClassCastException
	{
		return (T) valueObject;
	}

	/**
	 * @param value should not be {@code null}!
	 * @return
	 */
	public abstract String toString(T value);

	/**
	 * Converts the given {@code <T>} value to byte[] representation
	 * 
	 * @param value
	 * @return a byte[] or null in case of an error
	 * @throws IOException if an I/O error happens
	 */
	public byte[] toBytes(T value) throws IOException
	{
		BitOutputStream bos = null;
		try
		{
			ByteArrayOutputStream baos = new ByteArrayOutputStream(); 
			bos = new BitWrapOutputStream(baos);
			writeValue(value, bos);
			bos.flush();
			return baos.toByteArray();
		}
		finally
		{
			StreamHelpers.SilentClose(bos);
		}
	}

	/**
	 * Convert the given byte[] representation to a value of type {@code <T>}
	 * 
	 * @param bytes
	 * @return the value
	 * @throws IOException if an I/O error happens
	 */
	public T fromBytes(byte[] bytes) throws IOException
	{
		BitInputStream bis = null;
		try
		{
			bis = new BitWrapInputStream(new ByteArrayInputStream(bytes));
			return readValue(bis);
		}
		finally
		{
			StreamHelpers.SilentClose(bis);
		}
	}

	/**
	 * @param valueSet should not be {@code null}
	 * @param bitStream should not be {@code null}
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws NullPointerException if the valueSet or the bitStream is {@code null}
	 */
	public final void retrieveAndWriteValue(ValueSet<?> valueSet, BitOutputStream bitStream) throws IOException, IllegalArgumentException, NullPointerException
	{
		writeValue(retrieveValue(valueSet), bitStream);
	}

	/**
	 * Writes the given Object value to the given {@link BitOutputStream}.
	 * The value will be casted to type {@code <T>}.
	 *
	 * @param value the value to write, given as an {@link Object}, may be {@code null} if column is optional
	 * @param bitStream the {@link BitOutputStream} to write to, must not be {@code null}
	 * @throws ClassCastException if the Object cannot be casted to type {@code <T>}
	 * @throws NullPointerException if value is {@code null} on an non-optional column, or if the bitStream is {@code null} 
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type {@code <T>}
	 * @see #convert(Object)
	 */
	public void writeObject(Object value, BitOutputStream bitStream) throws ClassCastException, NullPointerException, IOException, IllegalArgumentException
	{
		writeValue(convert(value), bitStream);
	}

	/**
	 * Writes the given {@code <T>} value to the given {@link BitOutputStream}.
	 *
	 * @param value the value to write may be {@code null} if column is optional
	 * @param bitStream the {@link BitOutputStream} to write to, must not be {@code null}
	 * @throws NullPointerException if value is {@code null} on an non-optional column, or if the bitStream is {@code null}
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 */
	public void writeValue(T value, BitOutputStream bitStream) throws NullPointerException, IOException, IllegalArgumentException
	{
		if(optional)
			bitStream.write(value != null); // write "presence"-bit
		else
		{
			if(value == null)
				throw new NullPointerException("Non-optional value is null!");
		}
		if(value != null)
		{
			validate(value); // just in case, throws IllegalArgumentException if invalid
			write(value, bitStream); // handled by subclass
		}
	}

	/**
	 * Writes the given (non-{@code null}) value to the given {@link BitOutputStream} without checks.
	 * If this is an optional column the "presence"-bit should already have been written to the
	 * bitStream before this method is called.
	 *
	 * @param value the value to be written, assumed to be non-{@code null}
	 * @param bitStream the {@link BitOutputStream} to write to, assumed to be non-{@code null}
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 */
	protected abstract void write(T value, BitOutputStream bitStream) throws IOException;
	
	/**
	 * @param valueSet the ValueSet in which to store the read value, should not be {@code null}
	 * @param bitStream the {@link BitInputStream} to read from, should not be {@code null}
	 * @throws IOException if an I/O error happens upon reading from the bitStream
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is, or when the read value is invalid
	 * @throws NullPointerException if value is {@code null} on an non-optional column, if the valueSet is {@code null}, or if the bitStream is {@code null}
	 */
	public final void readAndStoreValue(ValueSet<?> valueSet, BitInputStream bitStream) throws IOException, IllegalArgumentException, NullPointerException
	{
		storeValueUnchecked(valueSet, readValue(bitStream)); // we use storeValueUnchecked() instead of storeValue() because readValue() already performs all checks
	}

	/**
	 * Reads a value from the given {@link BitInputStream}.
	 *
	 * @param bitStream the {@link BitInputStream} to read from
	 * @return the value that was read, should not be {@code null}
	 * @throws IOException if an I/O error happens upon reading from the bitStream
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws NullPointerException if the read value is {@code null} on an non-optional column, or if the bitStream is {@code null}
	 */
	public final T readValue(BitInputStream bitStream) throws IOException, IllegalArgumentException, NullPointerException
	{
		T value = null;
		if(!optional || bitStream.readBit()) // in case of optional column: only read value if "presence"-bit is true
		{
			value = read(bitStream);
			if(value == null)
				throw new NullPointerException(optional ? "Read null value even though presence-bit was set to true!" : "Non-optional value is null!");
			validate(value); // throws IllegalArgumentException if invalid
		}
		return value;
	}

	/**
	 * Reads a value from the given {@link BitInputStream} without checks.
	 *
	 * @param bitStream the {@link BitInputStream} to read from, assumed to be non-{@code null}
	 * @return the read value
	 * @throws IOException if an I/O error happens upon reading from the bitStream
	 */
	protected abstract T read(BitInputStream bitStream) throws IOException;

	/**
	 * Perform checks (e.g.: not too big for size restrictions, no invalid content, etc.) on potential value, given as an {@link Object} which may need to be converted first.
	 * {@code null} values will be accepted only if the column is optional.
	 *
	 * @param value
	 * @return whether or not the value is valid
	 * @see #convert(Object)
	 */
	public final boolean isValidValueObject(Object valueObject)
	{
		try
		{
			return isValidValue(convert(valueObject));
		}
		catch(Exception e)
		{
			return false;
		}
	}
	
	/**
	 * Perform checks (e.g.: not too big for size restrictions, no invalid content, etc.) on potential value, given as a String to be parsed.
	 * When the given valueString is {@code null} or empty this is interpreted as a {@code null} value, which will be accepted only if the column is optional.
	 *
	 * @param value
	 * @return whether or not the value is valid
	 */
	public final boolean isValidValueString(String valueString)
	{
		try
		{
			return isValidValue(stringToValue(valueString));
		}
		catch(Exception e)
		{
			return false;
		}
	}

	/**
	 * Perform checks (e.g.: not too big for size restrictions, no invalid content, etc.) on potential value.
	 * {@code null} values will be accepted only if the column is optional.
	 *
	 * @param value
	 * @return whether or not the value is valid
	 */
	public final boolean isValidValue(T value)
	{
		if(value == null)
			return optional;
		try
		{
			validate(value);
		}
		catch(Exception e)
		{
			return false;
		}
		return true;
	}
	
	/**
	 * Perform checks on potential value (e.g.: not too big for size restrictions, no invalid content, etc.)
	 * Argument is assumed to be non-{@code null}! Hence, {@code null} checks should happen before any call of this method.
	 *
	 * @param value should not be {@code null}!
	 * @throws IllegalArgumentException	in case of invalid value
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
			out = new BitWrapOutputStream(rawOut);

			//Write value:
			writeValue(value, out);

			// Flush, close & get bytes:
			out.flush();
			out.close();

			//Input stream:
			in = new BitWrapInputStream(new ByteArrayInputStream(rawOut.toByteArray()));

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
			StreamHelpers.SilentClose(out);
			StreamHelpers.SilentClose(in);
		}
	}

	public T retrieveValueAsStoredBinary(ValueSet<?> valueSet)
	{
		return getValueAsStoredBinary(retrieveValue(valueSet));
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
	public final boolean isOptional()
	{
		return optional;
	}
	
	public final boolean isRequired()
	{
		return isRequired(false); // don't recurse by default
	}
	
	/**
	 * Checks whether this column, and if {@code recurse} is {@code true} also _all_ of its subcolumns, is non-optional.
	 * 
	 * @param recurse whether or not to check recursively if all subColumns are also a non-optional
	 * @return whether the column is (recursively) non-optional 
	 */
	public boolean isRequired(boolean recurse)
	{
		return !optional;
	}

	public String toString()
	{
		return	getTypeString() + "Column:" + name;
	}

	public String getSpecification()
	{
		TransactionalStringBuilder blr = new TransactionalStringBuilder();
		blr.append(toString());
		blr.append(" [");
		blr.openTransaction("; ");
		blr.append(optional ? "optional" : "required");
		if(this instanceof VirtualColumn)
			blr.append("virtual");
		blr.append("size: " + getMinimumSize() + (isVariableSize() ? ("-" + getMaximumSize()) : "") + " bits");
		blr.commitTransaction();
		blr.append("]");
		return blr.toString();
	}

	public String getTypeString()
	{
		return getType().getSimpleName();
	}

	public abstract Class<T> getType();
	
	public T retrieveValueCopy(Record record)
	{
		T value = retrieveValue(record);
		return (value == null ? null : copy(value));
	}
	
	/**
	 * @param value
	 * @return
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type {@code <T>}
	 * @see #convert(Object)
	 */
	public T copyObject(Object value) throws ClassCastException
	{
		return value != null ? copy(convert(value)) : null;
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
		return equals(obj, true, true);
	}

	/**
	 * Equality check, compares optionalness, type and size/content restrictions, AND if checkName is true also the column name
	 *
	 * @param obj object to compare this one with
	 * @param checkName whether or not to compare the column name
	 * @param whether or not to check the restrictions
	 * @return whether or not the given Object is an identical/equivalent Column
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj, boolean checkName, boolean checkRestrictions)
	{
		if(this == obj) // compare pointers first
			return true;
		if(this.getClass().isInstance(obj))
		{
			Column<T> that = (Column<T>) obj;
			if(this.optional != that.optional)
				return false;
			// Check names:
			if(checkName && !this.name.equals(that.name))
				return false;
			// Check virtual versions:
			if(!Objects.equals(this.virtualVersions, that.virtualVersions))
				return false;
			// Check defaultValue:
			if(!Objects.equals(this.defaultValue, that.defaultValue))
				return false;
			// Check restrictions (size/content):
			return !checkRestrictions || equalRestrictions(that);
		}
		else
			return false;
	}
	
	/**
	 * Checks if this column is compatible with another in terms of type, optionality & restrictions
	 * 
	 * @param another
	 * @return
	 */
	@SuppressWarnings("unchecked")
	protected boolean isCompatible(Column<?> another)
	{
		if(this.getClass().isInstance(another))
			return this.optional == another.optional && equalRestrictions((Column<T>) another);
		else
			return false;
	}

	protected abstract boolean equalRestrictions(Column<T> otherColumn);

	/**
	 * Accept a ColumnVisitor. The column is excepted to call one of the visitor's visit() methods.
	 * 
	 * @param visitor
	 */
	public abstract void accept(ColumnVisitor visitor);

	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + getTypeString().hashCode();
		hash = 31 * hash + name.hashCode();
		hash = 31 * hash + (optional ? 0 : 1);
		hash = 31 * hash + (virtualVersions == null ? 0 : virtualVersions.hashCode());
		hash = 31 * hash + (defaultValue == null ? 0 : defaultValue.hashCode());
		return hash;
	}

	/**
	 * Add a virtual version of this column.
	 * A VirtualColumn instance will be created with this column as the "real" sourceColumn
	 * and the given targetColumn as its "virtual" counterpart.
	 *
	 * Note: this should happen *before* the column is added to a schema! This is indirectly
	 * enforced due to the way {@link Schema#getColumns(boolean)} works.
	 *
	 * @param targetColumn
	 * @param valueMapper
	 */
	public <VT> void addVirtualVersion(Column<VT> targetColumn, VirtualColumn.ValueMapper<VT, T> valueMapper)
	{
		addVirtualVersion(new VirtualColumn<VT, T>(this, targetColumn, valueMapper));
	}
	
	/**
	 * Add the given VirtualColumn as a virtual version of this column.
	 * 
	 * @param virtualVersion
	 */
	protected <VT> void addVirtualVersion(VirtualColumn<VT, T> virtualVersion)
	{
		if(virtualVersion == null || virtualVersion.getSourceColumn() != this)
			throw new IllegalArgumentException("Invalid virtual column");
		if(virtualVersions == null)
			virtualVersions = new ArrayList<VirtualColumn<?,T>>();
		virtualVersions.add(virtualVersion);
	}

	/**
	 * Returns all virtual versions of this column (empty list if there are none).
	 *
	 * @return the virtualVersions
	 */
	public List<VirtualColumn<?, T>> getVirtualVersions()
	{
		return virtualVersions == null ? Collections.<VirtualColumn<?, T>> emptyList() : virtualVersions;
	}

	/**
	 * Returns a specific virtual version of this column (null if not found).
	 *
	 * @param targetColumnName
	 * @return
	 */
	public VirtualColumn<?, T> getVirtualVersion(String targetColumnName)
	{
		for(VirtualColumn<?, T> vCol : getVirtualVersions())
			if(vCol.getTargetColumn().getName().equals(targetColumnName))
				return vCol;
		return null;
	}
	
	/* (non-Javadoc)
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	@Override
	public int compare(ValueSet<?> lhs, ValueSet<?> rhs)
	{
		return lhs == null ?
				(rhs == null ? 0 : Integer.MIN_VALUE) :
				(rhs == null ? Integer.MAX_VALUE : compareValues(retrieveValue(lhs), retrieveValue(rhs)));
	}
	
	/**
	 * Alias for {@link #compare(ValueSet<?>, ValueSet<?>)}
	 * 
	 * @param vs1
	 * @param vs2
	 * @return comparison result
	 */
	public int retrieveAndCompareValues(ValueSet<?> vs1, ValueSet<?> vs2)
	{
		return compare(vs1, vs2);
	}
	
	/**
	 * @param vs (probably shouldn't be null, but if it we will compare the given value to null)
	 * @param value (may be null if column is optional)
	 * @return comparison result
	 */
	public int retrieveAndCompareToValue(ValueSet<?> vs, T value)
	{
		return compareValues(vs != null ? retrieveValue(vs) : null, value);
	}
	
	/**
	 * @param vs (probably shouldn't be null, but if it we will compare the given value to null)
	 * @param value (as object, may be null if column is optional)
	 * @return comparison result
	 * @throws IllegalArgumentException in case of a schema mismatch or invalid value
	 * @throws NullPointerException if value is null on an non-optional column
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type <T>
	 */
	public int retrieveAndCompareToObject(ValueSet<?> vs, Object value) throws ClassCastException
	{
		return compareValues(vs != null ? retrieveValue(vs) : null, convert(value));
	}
	
	/**
	 * @param lhs left-hand side value, possibly null
	 * @param rhs right-hand side value, possibly null 
	 * @return comparison result
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 * @see <a href="http://stackoverflow.com/a/128220/1084488">http://stackoverflow.com/a/128220/1084488</a>
	 */
	public int compareValues(T lhs, T rhs)
	{
		return lhs == null ?
				(rhs == null ? 0 : Integer.MIN_VALUE) :
				(rhs == null ? Integer.MAX_VALUE : compareNonNullValues(lhs, rhs));
	}
	
	/**
	 * Defines an order relationship over {@code <T>} objects based solely on their
	 * {@link Object#hashCode()} or {@link System#identityHashCode(Object)}.
	 * This order therefore has _no_ real semantics and only serves as a means to
	 * group together identical values or the {@link ValueSet}s that contain them,
	 * without also ranking them in a meaningful, "logical", or domain-specific order.
	 * When such ordering is needed a {@link ComparableColumn} must be used instead.
	 * 
	 * @param lhs left-hand side value, guaranteed non-null
	 * @param rhs right-hand side value, guaranteed non-null
	 * @return comparison result
	 */
	protected int compareNonNullValues(T lhs, T rhs)
	{
		if(lhs == rhs || lhs.equals(rhs))
			return 0;
		int lhsHash = lhs.hashCode();
		int rhsHash = rhs.hashCode();		
		return	(lhsHash != rhsHash) ?
					Integer.compare(lhsHash, rhsHash) :
					Integer.compare(System.identityHashCode(lhs), System.identityHashCode(rhs));
	}
	
	/**
	 * @return
	 */
	public Comparator<T> getValueComparator()
	{
		return new Comparator<T>()
		{
			@Override
			public int compare(T lhs, T rhs)
			{
				return compareValues(lhs, rhs);
			}
		};
	}

}
