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

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
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
	 * @return the Class of the column's type T
	 */
	public abstract Class<T> getType();

	/**
	 * @return a copy of this Column
	 */
	public abstract Column<T> copy();
	
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
	
	public final boolean hasDefautValue()
	{
		return defaultValue != null;
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
	
	/**
	 * Casts a given {@link Object} to type {@code <T>}.
	 * 
	 * @param valueObject as {@link Object}, or {@code null}
	 * @return value of type {@code <T>}, or {@code null} if given valueObject was {@code null}
	 * @throws ClassCastException
	 */
	@SuppressWarnings("unchecked")
	public final T cast(Object valueObject) throws ClassCastException
	{
		return (T) valueObject;
	}
	
	/**
	 * Converts a given a given {@link Object} to an instance of type {@code <T>}.
	 * 
	 * Default implementation only performs an unchecked cast.
	 * To be overridden by subclasses which need to perform additional conversion in order to accept a wider range of value types.
	 * 
	 * @param valueObject as {@link Object}, or {@code null}
	 * @return converted value of type {@code <T>}, or {@code null} if given valueObject was {@code null}
	 * @throws ClassCastException
	 */
	public T convert(Object valueObject) throws ClassCastException
	{
		return cast(valueObject);
	}
	
	/**
	 * Stores the given {@code <T>} value in this column on the given valueSet. Performs optionality check and validation.
	 * Note that this obviously means the existing value in this column will be replaced.
	 *
	 * @param valueSet the valueSet in which to store the value, may not be {@code null}
	 * @param value the value to store, may be {@code null} if column is optional
	 * @return the stored value
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is, or when the given value is invalid
	 * @throws NullPointerException if value is {@code null} on an non-optional column, or if the valueSet is {@code null}
	 */
	public T storeValue(ValueSet<?> valueSet, T value) throws IllegalArgumentException, NullPointerException, UnsupportedOperationException
	{
		// Check:
		if(value == null)
		{
			if(!optional)
				throw new NullPointerException("Cannot set null value for non-optional column \"" + getName() + "\"!");
		}
		else
			validate(value); // throws IllegalArgumentException if invalid
		// Store & return value:
		return storeValueUnchecked(valueSet, value);
	}
	
	/**
	 * @param valueSet the valueSet in which to store the value, may not be {@code null}
	 * @param value the value to store, may be {@code null} (which will wipe earlier non-{@code null} values)
	 * @return the stored value
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws NullPointerException if the valueSet is {@code null}
	 */
	private final T storeValueUnchecked(ValueSet<?> valueSet, T value) throws IllegalArgumentException, NullPointerException
	{
		valueSet.setValue(this, value); // also store null (to overwrite earlier non-null value)
		return value;
	}
	
	/**
	 * Stores the given Object value (converted to an instance of type {@code <T>}) in this column on the given ValueSet.
	 * Note that this obviously means the existing value in this column will be replaced.
	 *
	 * @param valueSet {@link ValueSet} to store the value in, should not be {@code null}
	 * @param valueObject value to store, given as an {@link Object} (may be converted first), is allowed to be {@code null} only if column is optional
	 * @return the stored value
	 * @throws IllegalArgumentException in case of a columnSet mismatch or invalid value
	 * @throws NullPointerException if value is null on an non-optional column
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type {@code <T>}
	 * @see #convert(Object)
	 */
	public T storeObject(ValueSet<?> valueSet, Object valueObject) throws IllegalArgumentException, NullPointerException, ClassCastException
	{
		return storeObject(valueSet, valueObject, true); // convert to T!
	}
	
	/**
	 * Stores the given Object value ({@code convert}ed or simply casted to an instance of type {@code <T>}) in this column on the given ValueSet.
	 * Note that this obviously means the existing value in this column will be replaced.
	 *
	 * @param valueSet {@link ValueSet} to store the value in, should not be {@code null}
	 * @param valueObject value to store, given as an {@link Object} (may be converted first), is allowed to be {@code null} only if column is optional
	 * @param convert whether to {@link #convert(Object)} or simply {@link #cast(Object)} the given {@code valueObject}
	 * @return the stored value
	 * @throws IllegalArgumentException in case of a columnSet mismatch or invalid value
	 * @throws NullPointerException if value is null on an non-optional column
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type {@code <T>}
	 * @see #cast(Object)
	 * @see #convert(Object)
	 */
	public T storeObject(ValueSet<?> valueSet, Object valueObject, boolean convert) throws IllegalArgumentException, NullPointerException, ClassCastException
	{
		return storeValue(valueSet, convert ? convert(valueObject) : cast(valueObject)); // convert or cast to T
	}
	
	/**
	 * Converts the given String representation to a value of type {@code <T>} and stores it in this column on the given ValueSet.
	 * Note that this obviously means the existing value in this column will be replaced.
	 * 
	 * @param valueSet {@link ValueSet} to store the parsed value in, should not be {@code null}
	 * @param valueString may be {@code null} or empty {@code String} but both cases treated as representing a {@code null} value
	 * @return the stored value
	 * @throws ParseException
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is, or when the parsed value is invalid
	 * @throws NullPointerException if the parsed value is {@code null} on an non-optional column, or if the valueSet is {@code null}
	 */
	public T storeString(ValueSet<?> valueSet, String valueString) throws ParseException, IllegalArgumentException, NullPointerException
	{
		return storeValue(valueSet, stringToValue(valueString));
	}
	
	/**
	 * Converts the given binary representation to a value of type {@code <T>} and stores it in this column on the given ValueSet.
	 * Note that this obviously means the existing value in this column will be replaced.
	 * 
	 * @param valueSet {@link ValueSet} to store the value in, should not be {@code null}
	 * @param valueBytes binary representation of a column value, given as a {@code byte[]}
	 * @param lossless if {@code true} the value is expected to be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value is expected to be lossyly encoded
	 * @return the stored value
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is, or when the read value is invalid
	 * @throws NullPointerException if the given {@code byte[]} is {@code null}, if the read value is {@code null} on an non-optional column, or if the valueSet is {@code null}
	 * @throws IOException if an I/O error happens
	 * @see #fromBytes(byte[], boolean)
	 */
	public T storeBytes(ValueSet<?> valueSet, byte[] valueBytes, boolean lossless) throws IllegalArgumentException, NullPointerException, IOException
	{
		return storeValueUnchecked(valueSet, fromBytes(valueBytes, lossless)); // we use storeValueUnchecked() instead of storeValue() because readValue() (called by fromBytes()) already performs all checks
	}
	
	/**
	 * Converts the given binary representation to a value of type {@code <T>} and stores it in this column on the given ValueSet.
	 * Note that this obviously means the existing value in this column will be replaced.
	 * 
	 * @param valueSet {@link ValueSet} to store the value in, should not be {@code null}
	 * @param bytes binary representation of a column value, given as a {@link BitArray}
	 * @param lossless if {@code true} the value is expected to be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value is expected to be lossyly encoded
	 * @return the stored value
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is, or when the read value is invalid
	 * @throws NullPointerException if the given {@link BitArray} is {@code null}, if the read value is {@code null} on an non-optional column, or if the valueSet is {@code null}
	 * @throws IOException if an I/O error happens
	 * @see #fromBits(BitArray, boolean)
	 */
	public T storeBits(ValueSet<?> valueSet, BitArray valueBits, boolean lossless) throws IllegalArgumentException, NullPointerException, IOException
	{
		return storeValueUnchecked(valueSet, fromBits(valueBits, lossless)); // we use storeValueUnchecked() instead of storeValue() because readValue() (called by fromBits()) already performs all checks
	}
	
	/**
	 * (Re-)sets the value of this column in the given valueSet to {@code null}, even if the column is non-optional or has a non-{@code null} {@link Column#defaultValue}.
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
	 * Resets the value of this column in the given valueSet to the {@link #defaultValue} (usually {@code null}), even if the column is non-optional.
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
	 * If the value of this column in the given valueSet is currently {@code null} (i.e. the column is "empty") then it will be
	 * reset to the {@link #defaultValue} (assumed be be non-{@code null}), optionally only if the column is required (i.e. non-optional).
	 * Use with care!
	 * 
	 * @param valueSet
	 * @param onlyIfRequired whether to only reset required columns ((@code true}) or also optional ones ({@code false})
	 * 
	 * @see #resetValue(ValueSet)
	 */
	public final void resetIfEmpty(ValueSet<?> valueSet, boolean onlyIfRequired)
	{
		resetIfEmpty(valueSet, onlyIfRequired, false); // don't recurse by default
	}
	
	/**
	 * If the value of this column in the given valueSet is currently {@code null} (i.e. the column is "empty") then it will be
	 * reset to the {@link #defaultValue} (assumed be be non-{@code null}), optionally only if the column is required (i.e. non-optional)
	 * Use with care!
	 * 
	 * @param valueSet
	 * @param onlyIfRequired whether to only reset required columns ((@code true}) or also optional ones ({@code false})
	 * @param recurse whether or not to apply the operation recursively to all subColumns (only relevant if this is a composite Column)
	 * 
	 * @see #resetValue(ValueSet)
	 */
	public void resetIfEmpty(ValueSet<?> valueSet, boolean onlyIfRequired, boolean recurse)
	{
		if(defaultValue != null && !isValuePresent(valueSet) && !(optional && onlyIfRequired))
			resetValue(valueSet);
	}

	/**
	 * Retrieves previously stored value for this column from the given valueSet and casts it to the relevant native type (T).
	 *
	 * @param valueSet the {@link ValueSet} to retrieve the value from, should not be {@code null}
	 * @return stored value (may be {@code null})
	 * @throws NullPointerException if the given {@link ValueSet} is {@code null}
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 */
	@SuppressWarnings("unchecked")
	public <VS extends ValueSet<CS>, CS extends ColumnSet> T retrieveValue(VS valueSet) throws NullPointerException, IllegalArgumentException
	{
		if(valueSet == null)
			throw new NullPointerException("valueSet is null!");
		return (T) valueSet.getValue(this);
	}
	
	/**
	 * Retrieves previously stored value for this column from the given valueSet and converts it to a String representation.
	 * 
	 * @param valueSet should not be {@code null}
	 * @return a {@link String} representation of the value retrieved from the valueSet for this Column, or {@code null} if the valueSet does not contain a value for this Column
	 * @see #valueToString(Object)
	 */
	public String retrieveValueAsString(ValueSet<?> valueSet)
	{
		return valueToString(retrieveValue(valueSet));
	}
	
	/**
	 * Retrieves previously stored value for this column from the given valueSet and converts it to a String representation.
	 * 
	 * @param valueSet should not be {@code null}
	 * @param emptyForNull whether to return "" ({@code true}) or {@code null} ({@code false}) if the valueSet does not contain a value for this Column
	 * @return a {@link String} representation of the value retrieved from the valueSet for this Column, or {@code null} if the valueSet did not contain a value for this Column and {@code emptyForNull} was {@code false}
	 * @see #valueToString(Object,boolean)
	 */
	public String retrieveValueAsString(ValueSet<?> valueSet, boolean emptyForNull)
	{
		return valueToString(retrieveValue(valueSet), emptyForNull);
	}
	
	/**
	 * Retrieves previously stored value for this column from the given valueSet, converts it to a binary representation and returns the result as a {@code byte[]}.
	 * 
	 * @param valueSet the {@link ValueSet} to retrieve the value from, should not be {@code null}
	 * @param lossless if {@code true} the value will be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value will be encoded lossyly
	 * @return stored value as a {@code byte[]}
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws IOException if an I/O error happens
	 * @see #toBytes(Object, boolean)
	 */
	public <VS extends ValueSet<CS>, CS extends ColumnSet> byte[] retrieveValueAsBytes(VS valueSet, boolean lossless) throws IllegalArgumentException, IOException
	{
		return toBytes(retrieveValue(valueSet), lossless);
	}
	
	/**
	 * Retrieves previously stored value for this column from the given valueSet, converts it to a binary representation and returns the result as a {@link BitArray}.
	 * 
	 * @param valueSet the {@link ValueSet} to retrieve the value from, should not be {@code null}
	 * @param lossless if {@code true} the value will be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value will be encoded lossyly
	 * @return stored value as a {@link BitArray}
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws IOException if an I/O error happens
	 * @see #toBits(Object, boolean)
	 */
	public <VS extends ValueSet<CS>, CS extends ColumnSet> BitArray retrieveValueAsBits(VS valueSet, boolean lossless) throws IllegalArgumentException, IOException
	{
		return toBits(retrieveValue(valueSet), lossless);
	}

	/**
	 * Checks whether a non-{@code null} value for this column is set in the given valueSet.
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
	 * Checks, possibly recursively, whether a non-{@code null} value for this column is set in the given valueSet.
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
	 * Checks whether this column either has a non-{@code null} value in the given valueSet or is optional.
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
	 * Checks, possibly recursively, whether this column either has a non-{@code null} value in the given valueSet or is optional.
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
	 * Checks whether the value for this column in the given valueSet equals the column's {@link defaultValue}.
	 *
	 * @param valueSet should not be {@code null}
	 * @return whether or not a non-{@code null} value is set
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	public boolean isValueDefault(ValueSet<?> valueSet) throws IllegalArgumentException
	{
		return Objects.deepEquals(retrieveValue(valueSet), defaultValue);
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
	 * @param recurse whether or not to check recursively if all subColumns also have valid values (only relevant if this is a composite Column)
	 * @return whether the value currently contained by the valueSet for this column is valid (note that a {@code null} value is valid if the column is optional)
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	public boolean isValueValid(ValueSet<?> valueSet, boolean recurse) throws IllegalArgumentException
	{
		return isValidValue(retrieveValue(valueSet));
	}

	/**
	 * Parses a string representation of a value to produce a instance of {@code <T>}.
	 * Accepts {@code null} or empty {@code String}s but treats them as representing a {@code null} value.
	 * If no {@code null} or empty {@code String}s are expected it may be better to call {@link #parse(String)} instead.
	 * 
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
	 * Parses a string representation of a value to produce a instance of {@code <T>}.
	 * Does *not* accept {@code null} or empty {@code String}s, if these are expected it may be better to use {@link #stringToValue(String)}.
	 * 
	 * @param valueString the {@link String} to parse, should be neither {@code null} nor empty {@code String} (as those both represent a {@code null} value)
	 * @return the parsed value as type {@code <T>}
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 */
	public abstract T parse(String valueString) throws ParseException, IllegalArgumentException, NullPointerException;
	
	/**
	 * @param value may be {@code null}, in which case {@code null} is returned
	 * @return a {@link String} representation of the value, or {@code null} if the value was {@code null}
	 */
	public String valueToString(T value)
	{
		return valueToString(value, false);
	}
	
	/**
	 * @param value may be {@code null}, in which case "" or {@code null} is returned depending on {@code emptyForNull}
	 * @param emptyForNull whether to return "" ({@code true}) or {@code null} ({@code false}) in case of a {@code null} {@code value}
	 * @return a {@link String} representation of the given {@code value}, or {@code null} if the value was {@code null} and {@code emptyForNull} was {@code false}
	 */
	public String valueToString(T value, boolean emptyForNull)
	{
		if(value != null)
			return toString(value);
		else
			return emptyForNull ? "" : null;
	}

	/**
	 * @param valueObject may be {@code null}, in which case {@code null} is returned
	 * @param convert whether to {@link #convert(Object)} or simply {@link #cast(Object)} the given {@code valueObject}
	 * @return a String representation of the valueObject, or {@code null} if the valueObject was {@code null}
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type {@code <T>}
	 * @see #convert(Object)
	 * @see #cast(Object)
	 */
	public String objectToString(Object valueObject, boolean convert) throws ClassCastException
	{
		return valueToString(convert ? convert(valueObject) : cast(convert));
	}

	/**
	 * @param value should not be {@code null}!
	 * @return
	 */
	public abstract String toString(T value);

	/**
	 * Converts the given {@code <T>} value to binary representation and returns the result as a {@code byte[]}.
	 * 
	 * @param value
	 * @param lossless if {@code true} the value will be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value will be encoded lossyly
	 * @return a byte[] array
	 * @throws IOException if an I/O error happens
	 */
	public byte[] toBytes(T value, boolean lossless) throws IOException
	{
		BitOutputStream bos = null;
		try
		{
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			bos = new BitWrapOutputStream(baos);
			writeValue(value, bos, lossless);
			bos.flush();
			return baos.toByteArray();
		}
		finally
		{
			StreamHelpers.SilentClose(bos);
		}
		/*//Alternative implementation:
		return toBits(value, lossless).toByteArray();*/
	}
	
	/**
	 * Converts the given {@code <T>} value to binary representation and returns the result as a {@link BitArray}.
	 * 
	 * @param value
	 * @param lossless if {@code true} the value will be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value will be encoded lossyly
	 * @return a {@link BitArray}
	 * @throws IOException if an I/O error happens
	 */
	public BitArray toBits(T value, boolean lossless) throws IOException
	{
		BitArrayOutputStream bitsOut = null;
		try
		{
			bitsOut = new BitArrayOutputStream();
			writeValue(value, bitsOut, lossless);
			bitsOut.flush();
			return bitsOut.toBitArray();
		}
		finally
		{
			StreamHelpers.SilentClose(bitsOut);
		}
	}

	/**
	 * Convert the given binary representation to a value of type {@code <T>}.
	 * 
	 * @param bytes binary representation of a column value, given as a {@code byte[]}
	 * @param lossless if {@code true} the value is expected to be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value is expected to be lossyly encoded
	 * @return the value
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws IOException if an I/O error happens
	 */
	public T fromBytes(byte[] bytes, boolean lossless) throws IllegalArgumentException, IOException
	{
		BitInputStream bis = null;
		try
		{
			bis = new BitWrapInputStream(new ByteArrayInputStream(bytes));
			return readValue(bis, lossless); // validates value
		}
		finally
		{
			StreamHelpers.SilentClose(bis);
		}
		/*//Alternative implementation:
		return fromBits(BitArray.FromBytes(bytes), lossless);*/
	}
	
	/**
	 * Convert the given binary representation to a value of type {@code <T>}.
	 * 
	 * @param bits binary representation of a column value, given as a {@link BitArray}
	 * @param lossless if {@code true} the value is expected to be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value is expected to be lossyly encoded
	 * @return the value
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws IOException if an I/O error happens
	 */
	public T fromBits(BitArray bits, boolean lossless) throws IllegalArgumentException, IOException
	{
		BitInputStream bis = null;
		try
		{
			bis = new BitArrayInputStream(bits);
			return readValue(bis, lossless); // validates value
		}
		finally
		{
			StreamHelpers.SilentClose(bis);
		}
	}

	/**
	 * @param valueSet should not be {@code null}
	 * @param bitStream should not be {@code null}
	 * @param lossless if {@code true} the value will be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value will be encoded lossyly
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 * @throws NullPointerException if the valueSet or the bitStream is {@code null}
	 */
	public final void retrieveAndWriteValue(ValueSet<?> valueSet, BitOutputStream bitStream, boolean lossless) throws IOException, IllegalArgumentException, NullPointerException
	{
		writeValue(retrieveValue(valueSet), bitStream, lossless);
	}

	/**
	 * Writes the given Object value to the given {@link BitOutputStream}.
	 * The value will be casted to type {@code <T>}.
	 *
	 * @param value the value to write, given as an {@link Object} (will be casted, not converted), may be {@code null} if column is optional
	 * @param bitStream the {@link BitOutputStream} to write to, must not be {@code null}
	 * @param lossless if {@code true} the value will be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value will be encoded lossyly
	 * @throws ClassCastException if the Object cannot be casted to type {@code <T>}
	 * @throws NullPointerException if value is {@code null} on an non-optional column, or if the bitStream is {@code null} 
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 * @throws ClassCastException when the value cannot be casted to the column's type {@code <T>}
	 * @see #cast(Object)
	 */
	public void writeObject(Object value, BitOutputStream bitStream, boolean lossless) throws ClassCastException, NullPointerException, IOException, IllegalArgumentException
	{
		writeValue(cast(value), bitStream, lossless);
	}

	/**
	 * Writes the given {@code <T>} value to the given {@link BitOutputStream}.
	 *
	 * @param value the value to write, may be {@code null} if column is optional
	 * @param bitStream the {@link BitOutputStream} to write to, must not be {@code null}
	 * @param lossless if {@code true} the value will be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value will be encoded lossyly
	 * @throws NullPointerException if value is {@code null} on an non-optional column, or if the bitStream is {@code null}
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 */
	public void writeValue(T value, BitOutputStream bitStream, boolean lossless) throws NullPointerException, IOException, IllegalArgumentException
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
			write(value, bitStream, lossless); // handled by subclass
		}
	}

	/**
	 * Writes the given (non-{@code null}) value to the given {@link BitOutputStream} without checks.
	 * If this is an optional column the "presence"-bit should already have been written to the
	 * bitStream before this method is called.
	 *
	 * @param value the value to be written, assumed to be non-{@code null}
	 * @param bitStream the {@link BitOutputStream} to write to, assumed to be non-{@code null}
	 * @param lossless if {@code true} the value will be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value will be encoded lossyly
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 */
	protected abstract void write(T value, BitOutputStream bitStream, boolean lossless) throws IOException;
	
	/**
	 * @param valueSet the ValueSet in which to store the read value, should not be {@code null}
	 * @param bitStream the {@link BitInputStream} to read from, should not be {@code null}
	 * @param lossless if {@code true} the value is expected to be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value is expected to be lossyly encoded
	 * @throws IOException if an I/O error happens upon reading from the bitStream
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is, or when the read value is invalid
	 * @throws NullPointerException if value is {@code null} on an non-optional column, if the valueSet is {@code null}, or if the bitStream is {@code null}
	 */
	public final void readAndStoreValue(ValueSet<?> valueSet, BitInputStream bitStream, boolean lossless) throws IOException, IllegalArgumentException, NullPointerException
	{
		storeValueUnchecked(valueSet, readValue(bitStream, lossless)); // we use storeValueUnchecked() instead of storeValue() because readValue() already performs all checks
	}

	/**
	 * Reads a value from the given {@link BitInputStream}.
	 *
	 * @param bitStream the {@link BitInputStream} to read from
	 * @param lossless if {@code true} the value is expected to be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value is expected to be lossyly encoded
	 * @return the value that was read, should not be {@code null}
	 * @throws IOException if an I/O error happens upon reading from the bitStream
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws NullPointerException if the read value is {@code null} on an non-optional column, or if the bitStream is {@code null}
	 */
	public final T readValue(BitInputStream bitStream, boolean lossless) throws IOException, IllegalArgumentException, NullPointerException
	{
		T value = null;
		if(!optional || bitStream.readBit()) // in case of optional column: only read value if "presence"-bit is true
		{
			value = read(bitStream, lossless);
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
	 * @param lossless if {@code true} the value is expected to be losslessly encoded, if {@code false} (and {@link #canBeLossy()} returns {@code true}) the value is expected to be lossyly encoded
	 * @return the read value
	 * @throws IOException if an I/O error happens upon reading from the bitStream
	 */
	protected abstract T read(BitInputStream bitStream, boolean lossless) throws IOException;

	/**
	 * Perform checks (e.g.: not too big for size restrictions, no invalid content, etc.) on potential value, given as an {@link Object} which may need to be converted first.
	 * {@code null} values will be accepted only if the column is optional.
	 *
	 * @param value
	 * @param convert whether to {@link #convert(Object)} or simply {@link #cast(Object)} the given {@code valueObject}
	 * @return whether or not the value is valid
	 * @see #convert(Object)
	 * @see #cast(Object)
	 */
	public final boolean isValidValueObject(Object valueObject, boolean convert)
	{
		try
		{
			return isValidValue(convert ? convert(valueObject) : cast(valueObject));
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
	
	public T retrieveValueCopy(Record record)
	{
		T value = retrieveValue(record);
		return (value == null ? null : copy(value));
	}
	
	/**
	 * @param valueObject
	 * @param convert whether to {@link #convert(Object)} or simply {@link #cast(Object)} the given {@code valueObject}
	 * @return
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type {@code <T>}
	 * @see #convert(Object)
	 * @see #cast(Object)
	 */
	public T copyObject(Object valueObject, boolean convert) throws ClassCastException
	{
		return valueObject != null ? copy(convert ? convert(valueObject) : cast(valueObject)) : null;
	}

	/**
	 * Returns a (deep, depending on necessity) copy of the value
	 *
	 * @param value - assumed to be non-null!
	 * @return
	 */
	protected abstract T copy(T value);
	
	/**
	 * Checks whether or not this column supports lossy (binary) encoding of its values.
	 * "Lossy" (as opposed to "Lossless") encoding means that values may sustain information loss or reduced precision.
	 * 
	 * The default implementation relies on comparing the minimum and maximum size of lossy versus lossless binary representations.
	 * Subclasses may override this with more efficient implementations.
	 * 
	 * @return
	 */
	public boolean canBeLossy()
	{
		return (getMinimumValueSize(false) != getMinimumValueSize(true)) || (getMaximumValueSize(false) != getMaximumValueSize(true)); 
	}
	
	/**
	 * Checks whether or not this column will always encoding its values losslessly (i.e. never lossyly).
	 * "Lossy" (as opposed to "Lossless") encoding means that values may sustain information loss or reduced precision.
	 * 
	 * @return
	 */
	public boolean isAlwaysLossless()
	{
		return !canBeLossy();
	}
	
	/**
	 * Simulates the information loss sustained by converting the given value to lossy binary encoding and back.
	 * If this column does not support lossy encoding (i.e. {@link #canBeLossy()} returns {@code false}) the value is returned as-is.
	 * 
	 * @param value
	 * @return value as retrieved from lossy binary representation
	 */
	public T toLossy(T value)
	{
		if(!canBeLossy())
			return value;
		// else:
		try
		{
			// Convert to lossy binary representation and then back to a T value to return:
			return fromBytes(toBytes(value, false), false); // false: not lossless --> lossy
			/*//Alternative implementation:
			return fromBits(toBits(value, false), false); // false: not lossless --> lossy*/
		}
		catch(Exception e)
		{
			System.err.println("Error in getLossyEncodedValue(Record): " + e.getLocalizedMessage());
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * <p>
	 * Retrieves previously stored value for this column from the given valueSet.
	 * If {@code asLossy} is {@code true}, {@link #canBeLossy()} returns {@code true}, *and* if the valueSet is not already in a lossy state, then
	 * (and only then) the retrieved value is being returned as if retrieved from a lossy binary representation.</p>
	 * <p>
	 * The difference between calling {@code retrieveValue(valueSet, true)} and {@link #retrieveValueAsLossy(ValueSet)} is that the latter does not involves a
	 * call to {@link ValueSet#isLossy()} (which may itself call {@link #retrieveValueAsLossy(ValueSet)}, hence we need both methods to avoid endless loops).</p>
	 *
	 * @param valueSet the {@link ValueSet} to retrieve the value from, should not be {@code null}
	 * @param asLossy whether or not to simulate the information loss sustained by converting the retrieved value to lossy binary encoding and back
	 * @return stored value (may be {@code null}), possibly made lossy
	 * @throws IllegalArgumentException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 */
	public <VS extends ValueSet<CS>, CS extends ColumnSet> T retrieveValue(VS valueSet, boolean asLossy) throws IllegalArgumentException
	{
		if(!asLossy || !canBeLossy() || valueSet.isLossy() /*whole ValueSet is already lossy*/)
			return retrieveValue(valueSet);
		else
			return retrieveValueAsLossy(valueSet);
	}
	
	/**
	 * Retrieves previously stored value for this column from the given valueSet and simulates the
	 * information loss sustained by converting the value to lossy binary encoding and back.
	 * If this column does not support lossy encoding (i.e. {@link #canBeLossy()} returns {@code false}) the retrieved value is returned as-is.
	 * 
	 * @param valueSet
	 * @return value as retrieved from lossy binary representation
	 */
	public <VS extends ValueSet<CS>, CS extends ColumnSet> T retrieveValueAsLossy(VS valueSet)
	{
		return toLossy(retrieveValue(valueSet));
	}
	
	/**
	 * Returns the maximum effective number of bits values for this column take up
	 * when written to the least-efficient (likely lossless) binary representation,
	 * including the presence-bit in case of an optional column.
	 *
	 * @return
	 */
	public final int getMaximumSize()
	{
		return canBeLossy() ?
			// it is logical to assume that the maximum size for lossless encoding will always be bigger than that of lossy encoding, ...
			Math.max(getMaximumSize(true), getMaximumSize(false)) /* ... but we check to be sure */ :
			getMaximumSize(true);
	}

	/**
	 * Returns the maximum effective number of bits values for this column take up
	 * when written to a binary representation, including the presence-bit in case
	 * of an optional column.
	 *
	 * @param lossless whether to assume lossless ({@code true}) or lossy ({@code false}) value encoding
	 * @return
	 */
	public final int getMaximumSize(boolean lossless)
	{
		return (optional ? 1 : 0) + getMaximumValueSize(lossless);
	}

	/**
	 * Returns the maximum number of bits values for this column take up when written to
	 * a binary representation, _without_ the presence-bit in case of an optional column.
	 *
	 * @param lossless if {@code true} the returned maximum size is that of a losslessly encoded value, if {@code false} the returned maximum size is that of a lossyly encoded value
	 * @return
	 */
	protected abstract int getMaximumValueSize(boolean lossless);

	/**
	 * Returns the minimum effective number of bits values for this column take
	 * up when written to a most-efficient (possibly lossy) binary representation,
	 * including the presence-bit in case of an optional column.
	 *
	 * @return
	 */
	public final int getMinimumSize()
	{
		return canBeLossy() ?
			// it is logical to assume that the minimum size for lossy encoding will always be smaller than that of lossless encoding, ...
			Math.min(getMinimumSize(true), getMinimumSize(false)) /* ... but we check to be sure */ :
			getMinimumSize(true);
	}
	
	/**
	 * Returns the minimum effective number of bits values for this column take up when written to a binary representation, including the presence-bit in case of an optional column.
	 *
	 * @param lossless whether to assume lossless ({@code true}) or lossy ({@code false}) value encoding
	 * @return
	 */
	public final int getMinimumSize(boolean lossless)
	{
		if(optional)
			return 1;
		else
			return getMinimumValueSize(lossless);
	}

	/**
	 * Returns the minimum number of bits values for this column take up when written to a binary representation, _without_ the presence-bit in case of an optional column.
	 *
	 * @param lossless if {@code true} the returned minimum size is that of a losslessly encoded value, if {@code false} the returned minimum size is that of a lossyly encoded value
	 * @return
	 */
	protected abstract int getMinimumValueSize(boolean lossless);

	/**
	 * @return whether or not the size taken up by binary stored values for this column varies at run-time (i.e. depending on input)
	 */
	public boolean isVariableSize()
	{
		return isVariableSize(false) || isVariableSize(true);
	}
	
	/**
	 * @param lossless whether to assume lossless ({@code true}) or lossy ({@code false}) value encoding
	 * @return whether or not the size taken up by binary stored values for this column varies at run-time (i.e. depending on input)
	 */
	public boolean isVariableSize(boolean lossless)
	{
		return optional ? true : (getMinimumValueSize(lossless) != getMaximumValueSize(lossless));
	}
	
	/**
	 * Accept a ColumnVisitor. The column is excepted to call one of the visitor's visit() methods.
	 * 
	 * @param visitor
	 */
	public abstract void accept(ColumnVisitor visitor);

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
	 * @param value (as object, will be converted, may be null if column is optional)
	 * @return comparison result
	 * @throws IllegalArgumentException in case of a schema mismatch or invalid value
	 * @throws NullPointerException if value is null on an non-optional column
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type <T>
	 * @see #convert(Object)
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
	
	public String toString()
	{
		return getTypeString() + "Column:" + name;
	}
	
	public String getTypeString()
	{
		return getType().getSimpleName();
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
		if(canBeLossy())
			blr.append("lossy size: " + getMinimumSize(false) + (isVariableSize(false) ? ("-" + getMaximumSize(false)) : "") + " bits");
		blr.append((canBeLossy() ? "lossless " : "") + "size: " + getMinimumSize(true) + (isVariableSize(true) ? ("-" + getMaximumSize(true)) : "") + " bits");
		blr.commitTransaction();
		blr.append("]");
		return blr.toString();
	}
	
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

}
