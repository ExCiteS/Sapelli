/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Set;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.util.StringUtils;
import uk.ac.ucl.excites.sapelli.util.xml.XMLUtils;

/**
 * A class representing records of a certain Schema
 * 
 * @author mstevens
 */
public class Record
{
	
	// Statics-------------------------------------------------------
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
		values = new Object[schema.getNumberOfColumns()];
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
		for(Column<?> c : this.schema.getColumns())
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
			if(newSchema.getNumberOfColumns() != values.length)
				throw new IndexOutOfBoundsException("The new schema has a different number of columns than the number of values in this record!");
		}
		else
		{
			if(!schema.equals(newSchema, true, true)) // also checks number of columns
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
		setValue(schema.getColumnPosition(column), value);
	}
	
	private void setValue(int columnPosition, Object value)
	{
		if(columnPosition == Schema.UNKNOWN_COLUMN_POSITION)
			throw new IllegalArgumentException("Invalid column position (" + columnPosition + ")!");
		values[columnPosition] = value;
	}

	protected Object getValue(Column<?> column)
	{
		return getValue(schema.getColumnPosition(column));
	}
	
	protected Object getValue(String columnName)
	{
		return getValue(schema.getColumnPosition(columnName));
	}
	
	protected Object getValue(int columnIndex)
	{
		return values[columnIndex];
	}
	
	public boolean isFilled()
	{
		for(Column<?> c : schema.getColumns())
		{
			Object v = getValue(c);
			if(v == null && !c.isOptional())
				return false; //null value in non-optional column	
		}		
		return true;
	}
	
	public void writeToBitStream(BitOutputStream bitStream, Set<Column<?>> skipColumns) throws IOException
	{
		try
		{	//write fields:
			for(Column<?> c : schema.getColumns())
				if(skipColumns == null || !skipColumns.contains(c))
					c.retrieveAndWriteValue(this, bitStream);
		}
		catch(Exception e)
		{
			throw new IOException("Error on attempting to write record", e);
		}
	}
	
	public void readFromBitStream(BitInputStream bitStream, Set<Column<?>> skipColumns) throws IOException
	{
		try
		{	//read fields:
			for(Column<?> c : schema.getColumns())
				if(skipColumns == null || !skipColumns.contains(c))
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
	public int getSize()
	{
		BitOutputStream out = null;
		try
		{
			out = new BitOutputStream(new ByteArrayOutputStream());
			this.writeToBitStream(out, null);
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
		try
		{
			int hash = 1;
			hash = 31 * hash + schema.hashCode();
			hash = 31 * hash + Arrays.hashCode(this.toBytes());
			return hash;
		}
		catch(IOException ioe)
		{
			ioe.printStackTrace(System.err);
			return super.hashCode();
		}
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
			{	// Check if records have the same schema (object)
				if(this.schema != other.schema) // we could also use equals() here, but in principle we do not story duplicate schemas
					return false;
			}
			else
			{	// Only check if the number of columns matches (to avoid out or range errors below):
				if(this.schema.getNumberOfColumns() != other.schema.getNumberOfColumns())
					return false;
			}
			// Compare values for each column (using values as if decoded from binary stream):
			for(int i = 0; i < this.schema.getNumberOfColumns(); i++)
			{
				Object v1 = this.schema.getColumn(i).retrieveValueAsStoredBinary(this);
				Object v2 = other.schema.getColumn(i).retrieveValueAsStoredBinary(other);
				if(v1 != null)
				{
					if(!v1.equals(v2))
						return false;
				}
				else if(v2 != null)
					return false;
			}
			return true;
		}
		else
			return false;
	}
	
	@Override
	public String toString()
	{
		StringBuffer bff = new StringBuffer();
		bff.append(schema.toString());
		for(Column<?> c : schema.getColumns())
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
		return serialise(null);
	}
	
	/**
	 * Serialise a the Record to a String
	 * 
	 * @param skipColumns
	 * @return
	 */
	public String serialise(Set<Column<?>> skipColumns)
	{
		StringBuilder bldr = new StringBuilder();
		boolean first = true;
		for(Column<?> col : schema.getColumns())
		{
			// Separator:
			if(first)
				first = false;
			else
				bldr.append(SERIALISATION_SEPARATOR);
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
	 */
	public void parse(String serialisedRecord) throws Exception
	{
		parse(serialisedRecord, null);
	}
	
	/**
	 * Deserialise the values of a Record from a String
	 * 
	 * @param serialisedRecord
	 * @param skipColumns
	 * @throws Exception
	 */
	public void parse(String serialisedRecord, Set<Column<?>> skipColumns) throws ParseException, IllegalArgumentException, NullPointerException
	{
		String[] parts = serialisedRecord.split("\\" + SERIALISATION_SEPARATOR);
		if(parts.length != values.length)
			throw new IllegalArgumentException("Mismatch between number of serialised values (" + parts.length + ") and the number of columns in the schema (" + values.length + ").");
		int p = 0;
		for(Column<?> col : schema.getColumns())
		{
			if(skipColumns == null || !skipColumns.contains(col))
				col.parseAndStoreValue(this, StringUtils.deescape(parts[p], SERIALISATION_SEPARATOR, SERIALISATION_SEPARATOR_ESCAPE, SERIALISATION_SEPARATOR_ESCAPE_PREFIX));
			p++;
		}
	}
	
	public String toXML(int tabs)
	{
		StringBuilder bldr = new StringBuilder();
		bldr.append(StringUtils.addTabsFront(	"<" + TAG_RECORD + " " +
												Schema.ATTRIBUTE_SCHEMA_NAME + "=\"" + XMLUtils.escapeCharacters(schema.getName()) + "\" " +
												Schema.ATTRIBUTE_USAGE_ID + "=\"" + schema.getUsageID() + "\" " +
												Schema.ATTRIBUTE_USAGE_SUB_ID + "=\"" + schema.getUsageSubID() + "\" " +
												">\n", tabs));
		//TODO transmission/sent
		for(Column<?> c : schema.getColumns())
		{
			String columnName = XMLUtils.escapeCharacters(c.getName());
			String valueStr = c.retrieveValueAsString(this);
			if(valueStr != null)
				bldr.append(StringUtils.addTabsFront("<" + columnName + ">" + XMLUtils.escapeCharacters(valueStr) + "</" + columnName + ">\n", tabs + 1));
			else
			{
				bldr.append(StringUtils.addTabsFront(XMLUtils.comment(columnName + " is null") + "\n", tabs + 1));
			}
		}
		bldr.append(StringUtils.addTabsFront("</" + TAG_RECORD + ">", tabs));
		return bldr.toString();
	}
	
	public String toCSVRow(String separator)
	{
		StringBuilder bldr = new StringBuilder();
		for(Column<?> c : schema.getColumns())
		{
			if(c != schema.getColumn(0))
				bldr.append(separator);
			bldr.append(c.retrieveValueAsString(this));
		}
		return bldr.toString();
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
			this.writeToBitStream(out, null);
			
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
	
}
