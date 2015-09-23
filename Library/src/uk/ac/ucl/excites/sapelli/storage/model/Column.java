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
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLNameEncoder;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLUtils;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * Abstract class representing database schema/table column of generic type {@code T}.
 *
 * @param <T>
 * @author mstevens
 */
public abstract class Column<T> implements Serializable
{

	// STATICS-------------------------------------------------------
	private static final long serialVersionUID = 2L;

	static public final char ILLEGAL_NAME_CHAR_REPLACEMENT = '_';

	static public String SanitiseName(String name)
	{
		// Perform basic sanitation:
		//	Detect & replace the most common illegal characters with a simple underscore; except those that come last, which are just removed
		StringBuilder bldr = new StringBuilder();
		int prevNeedsReplace = 0;
		for(char c : name.toCharArray())
			switch(c)
			{
				// Not allowed anywhere:
				case '.'		:	/* not allowed at start of XML names, but nowhere in Sapelli column names because it is the RecordColumn.QUALIFIED_NAME_SEPARATOR */
				case ','		:	/* not allowed anywhere in XML names */
				case '?'		:	/* not allowed anywhere in XML names */
				case '!'		:	/* not allowed anywhere in XML names */
				case ':'		:	/* is allowed anywhere in XML names but we take it out anyway to avoid some XML tools recognising it as an XML namespace separator */
				case ';'		:	/* not allowed anywhere in XML names */
				case '\''		:	/* not allowed anywhere in XML names */
				case '"'		:	/* not allowed anywhere in XML names */
				case '/'		:	/* not allowed anywhere in XML names */
				case '\\'		:	/* not allowed anywhere in XML names */
				case '@'		:	/* not allowed anywhere in XML names */
				case '('		:	/* not allowed anywhere in XML names */
				case ')'		:	/* not allowed anywhere in XML names */
				case '['		:	/* not allowed anywhere in XML names */
				case ']'		:	/* not allowed anywhere in XML names */
				case '{'		:	/* not allowed anywhere in XML names */
				case '}'		:	/* not allowed anywhere in XML names */
				case '&'		:	/* not allowed anywhere in XML names */
				case '%'		:	/* not allowed anywhere in XML names */
				case '$'		:	/* not allowed anywhere in XML names */
				case '\u00A3'	:	/* (Pound sign) not allowed anywhere in XML names */
				case '+'		:	/* not allowed anywhere in XML names */
				case '*'		:	/* not allowed anywhere in XML names */
				case '#'		:	/* not allowed anywhere in XML names */
				case '|'		:	/* not allowed anywhere in XML names */
				case '~'		:	/* not allowed anywhere in XML names */
				case ' '		:	/* not allowed anywhere in XML names */
				case '\t'		:	/* not allowed anywhere in XML names */
				case '\r'		:	/* not allowed anywhere in XML names */
				case '\n'		:	/* not allowed anywhere in XML names */
					prevNeedsReplace++;
					break;

				// Not allowed at start, OK elsewhere:
				case '-'	:	/* not allowed at start of XML names */
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

	static public boolean IsValidName(String name)
	{
		return 	name.indexOf(ValueSetColumn.QUALIFIED_NAME_SEPARATOR /* '.' */) == -1 &&
				name.indexOf(':') == -1 &&
				XMLUtils.isValidName(name, XMLRecordsExporter.USES_XML_VERSION_11);
	}

	// DYNAMICS------------------------------------------------------
	public final String name;
	public final boolean optional;
	private List<VirtualColumn<?, T>> virtualVersions;

	public Column(String name, boolean optional)
	{
		if(!IsValidName(name))
			throw new IllegalArgumentException("Invalid column name (" + name + "), please use static the Column#SanitiseName method.");
		this.name = name;
		this.optional = optional;
	}

	/**
	 * @return a copy of this Column
	 */
	public abstract Column<T> copy();

	public void parseAndStoreValue(ValueSet<?> valueSet, String value) throws ParseException, IllegalArgumentException, NullPointerException
	{
		T parsedValue;
		if(value == null || value.isEmpty()) //empty String is treated as null
			parsedValue = null;
		else
			parsedValue = parse(value);
		storeValue(valueSet, parsedValue);
	}

	/**
	 * @param value the String to parse, is expected to be neither null nor ""!
	 * @return the parsed value
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 */
	public abstract T parse(String value) throws ParseException, IllegalArgumentException, NullPointerException;

	/**
	 * Stores the given Object value in this column on the given record.
	 *
	 * @param valueSet
	 * @param value (as object, may be null if column is optional)
	 * @throws IllegalArgumentException in case of a columnSet mismatch or invalid value
	 * @throws NullPointerException if value is null on an non-optional column
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type <T>
	 */
	@SuppressWarnings("unchecked")
	public void storeObject(ValueSet<?> valueSet, Object value) throws IllegalArgumentException, NullPointerException, ClassCastException
	{
		storeValue(valueSet, (T) convert(value));
	}
	
	/**
	 * Stores the given <T> value in this column on the given valueSet.
	 *
	 * @param valueSet
	 * @param value (may be null if column is optional)
	 * @throws IllegalArgumentException when this column is not part of the valueSet's columnSet, nor compatible with a column by the same name that is, or when the given value is invalid
	 * @throws NullPointerException if value is null on an non-optional column
	 */
	public void storeValue(ValueSet<?> valueSet, T value) throws IllegalArgumentException, NullPointerException, UnsupportedOperationException
	{
		if(value == null)
		{
			if(!optional)
				throw new NullPointerException("Cannot set null value for non-optional column \"" + getName() + "\"!");
		}
		else
			validate(value); // throws IllegalArgumentException if invalid
		valueSet.setValue(this, value); // also store null (to overwrite earlier non-values)
	}
	
	/**
	 * (Re-)sets the value of this column in the given record to {@code null}, even if the column is optional.
	 * Use with care!
	 * 
	 * @param record
	 */
	public void clearValue(Record record)
	{
		record.setValue(this, null);
	}

	/**
	 * Retrieves previously stored value for this column at a given valueSet and casts it to the relevant native type (T).
	 *
	 * @param valueSet
	 * @return stored value (may be null)
	 * @throws IllegalArgumentException when this column is not part of the valueSet's columnSet, nor compatible with a column by the same name that is
	 */
	@SuppressWarnings("unchecked")
	public T retrieveValue(ValueSet<?> valueSet) throws IllegalArgumentException
	{
		return (T) valueSet.getValue(this);
	}

	/**
	 * Checks whether a value (non-null) for this column is set in the given valueSet.
	 *
	 * @param value
	 * @return whether or not a (non-null) value is set
	 * @throws IllegalArgumentException when this column is not part of the record's schema, nor compatible with a column by the same name that is
	 */
	public boolean isValueSet(ValueSet<?> valueSet) throws IllegalArgumentException
	{
		return retrieveValue(valueSet) != null;
	}

	/**
	 * @param valueSet
	 * @return
	 */
	public String retrieveValueAsString(ValueSet<?> valueSet)
	{
		T value = retrieveValue(valueSet);
		if(value != null)
			return toString(value);
		else
			return null;
	}

	/**
	 * @param value
	 * @return a String representation of the value, or null if the value was null
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type <T>
	 */
	@SuppressWarnings("unchecked")
	public String objectToString(Object value) throws ClassCastException
	{
		if(value == null)
			return null;
		return toString((T) convert(value));
	}
	
	/**
	 * Does nothing by default.
	 * To be overridden by subclasses which need to perform additional conversion in order to accept a wider range of value types.
	 * 
	 * @param value possibly null
	 * @return
	 * @throws ClassCastException
	 */
	public Object convert(Object value) throws ClassCastException
	{
		return value;
	}

	/**
	 * @param value assumed to be non-null!
	 * @return
	 */
	public abstract String toString(T value);

	/**
	 * Convert the given <T> value to byte[] representation
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
			if(bos != null)
				try
				{
					bos.close();
				}
				catch(IOException ignore) { }
		}
	}

	/**
	 * Convert the given byte[] representation to a value of type <T>
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
			if(bis != null)
				try
				{
					bis.close();
				}
				catch(IOException ignore) { }
		}
	}

	/**
	 * @param valueSet
	 * @param bitStream
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public final void retrieveAndWriteValue(ValueSet<?> valueSet, BitOutputStream bitStream) throws IOException, IllegalArgumentException
	{
		writeValue(retrieveValue(valueSet), bitStream);
	}

	/**
	 * Writes the given Object value to the given {@link BitOutputStream}.
	 * The value will be casted to type <T>.
	 *
	 * @param value (may be null, if column is optional)
	 * @param bitStream the {@link BitOutputStream} to write to
	 * @throws ClassCastException if the Object cannot be casted to type <T>
	 * @throws NullPointerException if value is null on an non-optional column
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type <T>
	 */
	@SuppressWarnings("unchecked")
	public void writeObject(Object value, BitOutputStream bitStream) throws ClassCastException, NullPointerException, IOException, IllegalArgumentException
	{
		writeValue((T) convert(value), bitStream);
	}

	/**
	 * Writes the given <T> value to the given {@link BitOutputStream}.
	 *
	 * @param value (may be null, if column is optional)
	 * @param bitStream the {@link BitOutputStream} to write to
	 * @throws NullPointerException if value is null on an non-optional column
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
	 * Writes the given (non-null) value to the given {@link BitOutputStream} without checks.
	 *
	 * @param value assumed to be non-null!
	 * @param bitStream the {@link BitOutputStream} to write to
	 * @throws IOException if an I/O error happens upon writing to the bitStream
	 */
	protected abstract void write(T value, BitOutputStream bitStream) throws IOException;
	
	public final void readAndStoreValue(ValueSet<?> valueSet, BitInputStream bitStream) throws IOException, IllegalArgumentException, NullPointerException
	{
		storeValue(valueSet, readValue(bitStream));
	}

	/**
	 * Reads a value from the given {@link BitInputStream}.
	 *
	 * @param bitStream the {@link BitInputStream} to read from
	 * @return
	 * @throws IOException if an I/O error happens upon reading from the bitStream
	 * @throws IllegalArgumentException if the value does not pass the validation test
	 */
	public final T readValue(BitInputStream bitStream) throws IOException, IllegalArgumentException
	{
		T value = null;
		if(!optional || bitStream.readBit()) //in case of optional column: only read value if "presence"-bit is true
		{
			value = read(bitStream);
			if(value == null)
				throw new IOException(optional ? "Read null value even though presence-bit was set to true!" : "Non-optional value is null!");
			validate(value); //throws IllegalArgumentException if invalid
		}
		return value;
	}

	/**
	 * Reads a value from the given {@link BitInputStream} without checks.
	 *
	 * @param bitStream the {@link BitInputStream} to read from
	 * @return
	 * @throws IOException if an I/O error happens upon reading from the bitStream
	 */
	protected abstract T read(BitInputStream bitStream) throws IOException;

	/**
	 * Perform checks (e.g.: not too big for size restrictions, no invalid content, etc.) on potential value
	 *
	 * @param value
	 * @return whether or not the value is valid
	 */
	@SuppressWarnings("unchecked")
	public boolean isValidValue(Object value)
	{
		if(value == null)
			return optional == true;
		try
		{
			validate((T) convert(value));
		}
		catch(Exception e)
		{
			return false;
		}
		return true;
	}

	/**
	 * Perform checks (e.g.: not too big for size restrictions, no invalid content, etc.) on potential value, given as a String to be parsed
	 *
	 * @param value
	 * @return whether or not the value is valid
	 */
	public boolean isValidValueString(String valueStr)
	{
		if(valueStr == null || valueStr.isEmpty()) //empty String is treated as null
			return optional == true;
		try
		{
			return isValidValue(parse(valueStr));
		}
		catch(Exception e)
		{
			return false;
		}
	}

	/**
	 * Perform checks on potential value (e.g.: not too big for size restrictions, no invalid content, etc.)
	 * Argument is assumed to be non-null! Hence, null checks should happen before any call of this method.
	 *
	 * @param value
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
	public boolean isOptional()
	{
		return optional;
	}

	public String toString()
	{
		return	getTypeString() + "Column";
	}

	public String getSpecification()
	{
		return toString() + " [" +
		    				name + "; "
		    				+ (optional ? "optional" : "required") + "; "
		    				+ (this instanceof VirtualColumn ? "virtual; " : "")
		    				+ getMinimumSize() + (isVariableSize() ? "-" + getMaximumSize() : "") + " bits]";
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
	 * @throws ClassCastException when the value cannot be converted/casted to the column's type <T>
	 */
	@SuppressWarnings("unchecked")
	public T copyObject(Object value) throws ClassCastException
	{
		return value != null ? copy((T) convert(value)) : null;
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
			Column<T> other = (Column<T>) obj;
			if(this.optional != other.optional)
				return false;
			// Check names:
			if(checkName && !this.name.equals(other.name))
				return false;
			if(virtualVersions != null)
			{
				if(!virtualVersions.equals(other.virtualVersions))
					return false;
			}
			else if(other.virtualVersions != null)
				return false;
			// Check restrictions (size/content):
			return !checkRestrictions || equalRestrictions(other);
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

	public abstract void accept(ColumnVisitor visitor);

	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + getTypeString().hashCode();
		hash = 31 * hash + name.hashCode();
		hash = 31 * hash + (optional ? 0 : 1);
		hash = 31 * hash + (virtualVersions == null ? 0 : virtualVersions.hashCode());
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

}
