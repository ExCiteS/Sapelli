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
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLUtils;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * Abstract class representing database schema/table column of generic type {@code T}.  
 * 
 * @param <T>
 * @author mstevens
 */
/**
 * @author mstevens
 *
 * @param <T>
 */
public abstract class Column<T> implements Serializable
{
	
	// STATICS-------------------------------------------------------
	private static final long serialVersionUID = 2L;
	
	static public boolean IsValidName(String name)
	{
		return name.indexOf(RecordColumn.QUALIFIED_NAME_SEPARATOR) == -1 && XMLUtils.isValidName(name, XMLRecordsExporter.USES_XML_VERSION_11);
	}
	
	// DYNAMICS------------------------------------------------------
	private final Class<T> type;
	protected final String name;
	protected final boolean optional;
	protected List<VirtualColumn<?, T>> virtualVersions;
	
	public Column(Class<T> type, String name, boolean optional)
	{
		if(!IsValidName(name))
			throw new IllegalArgumentException("Invalid column name: " + name);
		this.type = type;
		this.name = name;
		this.optional = optional;
	}
	
	/**
	 * @return a copy of this Column
	 */
	public abstract Column<T> copy();
	
	public void parseAndStoreValue(Record record, String value) throws ParseException, IllegalArgumentException, NullPointerException
	{
		T parsedValue;
		if(value == null || value.isEmpty()) //empty String is treated as null
			parsedValue = null;
		else
			parsedValue = parse(value);
		storeValue(record, parsedValue);
	}
	
	/**
	 * @param value the String to parse, is expected to be neither null nor ""!
	 * @return the parsed value
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 */
	public abstract T parse(String value) throws ParseException, IllegalArgumentException, NullPointerException;
	
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
	public void storeValue(Record record, T value) throws IllegalArgumentException, NullPointerException, UnsupportedOperationException
	{
		if(!record.getSchema().containsColumn(this, false))
			throw new IllegalArgumentException("Schema mismatch.");
		if(value == null)
		{
			if(!optional)
				throw new NullPointerException("Cannot set null value for non-optional column \"" + getName() + "\"!");
		}
		else
			validate(value); //throws IllegalArgumentException if invalid
		record.setValue(this, value); // also store null (to overwrite earlier non-values)
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
		if(!record.getSchema().containsColumn(this, false))
			throw new IllegalArgumentException("Schema mismatch.");
		return (T) record.getValue(this);
	}
	
	/**
	 * Checks whether a value (non-null) for this column is set in the given record.
	 * 
	 * @param record
	 * @return whether or not a (non-null) value is set
	 */
	public boolean isValueSet(Record record)
	{
		return retrieveValue(record) != null;
	}
	
	/**
	 * @param record
	 * @return
	 */
	public String retrieveValueAsString(Record record)
	{
		T value = retrieveValue(record);
		if(value != null)
			return toString(value);
		else
			return null;
	}
	
	/**
	 * @param value
	 * @return a String representation of the value, or null if the value was null
	 */
	@SuppressWarnings("unchecked")
	public String objectToString(Object value)
	{
		if(value == null)
			return null;
		return toString((T) value);
	}
	
	/**
	 * @param value assumed to be non-null!
	 * @return
	 */
	public abstract String toString(T value);
	
	public final void retrieveAndWriteValue(Record record, BitOutputStream bitStream) throws IOException, IllegalArgumentException
	{
		writeValue(retrieveValue(record), bitStream);		
	}

	@SuppressWarnings("unchecked")
	public void writeObject(Object value, BitOutputStream bitStream) throws IOException, IllegalArgumentException
	{
		writeValue((T) value, bitStream);
	}
	
	/**
	 * Writes the given value to the given {@link BitOutputStream}.
	 * 
	 * @param value
	 * @param bitStream
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public void writeValue(T value, BitOutputStream bitStream) throws IOException, IllegalArgumentException
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
			validate(value); //just in case, throws IllegalArgumentException if invalid
			write(value, bitStream); //handled by subclass
		}
	}
	
	/**
	 * @param value assumed to be non-null!
	 * @param bitStream
	 * @throws IOException
	 */
	protected abstract void write(T value, BitOutputStream bitStream) throws IOException;
	
	public final void readAndStoreValue(Record record, BitInputStream bitStream) throws IOException, IllegalArgumentException, NullPointerException
	{
		storeValue(record, readValue(bitStream));
	}
	
	/**
	 * Reads a value from the given {@link BitInputStream}
	 * 
	 * @param bitStream
	 * @return
	 * @throws IOException
	 * @throws IllegalArgumentException
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
	 * @param bitStream
	 * @return
	 * @throws IOException
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
			validate((T) value);
		}
		catch(Exception iae)
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
		return type.getSimpleName();
	}
	
	public Class<T> getType()
	{
		return type;
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
		return equals(obj, false, true); // do not check name by default, but do check restrictions by default
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
	
	protected abstract boolean equalRestrictions(Column<T> otherColumn);
	
	public abstract void accept(ColumnVisitor visitor);
	
	@Override
    public int hashCode()
	{
		// DB4O issue workaround:
		if(type == null) //	TODO remove this at some point when we have abandoned DB4O for good
			return super.hashCode();
		// else:
		int hash = 1;
		hash = 31 * hash + type.getName().hashCode(); // don't use type.hashCode() as it is not stable (Class does not implement hashCode() so Object.hashCode() is executed)
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
		if(virtualVersions == null)
			virtualVersions = new ArrayList<VirtualColumn<?,T>>();
		virtualVersions.add(new VirtualColumn<VT, T>(this, targetColumn, valueMapper));
	}

	/**
	 * @return the virtualVersions
	 */
	protected List<VirtualColumn<?, T>> getVirtualVersions()
	{
		return virtualVersions == null ? Collections.<VirtualColumn<?, T>>emptyList() : virtualVersions;
	}
	
}
