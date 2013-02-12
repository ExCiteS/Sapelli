package uk.ac.ucl.excites.storage.model;

import java.util.Set;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;

/**
 * @author mstevens
 *
 */
@SuppressWarnings("rawtypes")
public class Record
{

	private Schema schema;
		
	public Record(Schema schema)
	{
		if(!schema.isSealed())
			throw new IllegalArgumentException("Schema must be sealed before records based on it can be created!");
		this.schema = schema;
	}
	
	/**
	 * Sets values, provided as strings (which can be empty or null for the optional columns only), in order of the columns
	 * 
	 * @param values
	 * @throws Exception
	 */
	public void setParsedValues(String... values) throws Exception
	{
		if(values == null || values.length == 0)
			throw new IllegalArgumentException("No values provided.");
		if(values.length != schema.getColumns().size())
			throw new IllegalArgumentException("Mismatch between number of values provided (" + values.length + ") and the number of columns in the schema (" + schema.getColumns().size() + "). Please remember to put null for empty optional columns.");
		int c = 0;
		for(String v : values)
		{
			setParsedValue(c, v);
			c++;
		}
	}
	
	public void setParsedValue(String columnName, String value) throws Exception
	{
		setParsedValue(schema.getColumn(columnName), value);
	}
	
	public void setParsedValue(int columnIndex, String value) throws Exception
	{
		setParsedValue(schema.getColumn(columnIndex), value);
	}
		
	public void setParsedValue(Column column, String value) throws Exception
	{
		column.parseAndStoreValue(this, value);
	}
	
	public Object get(String columnName)
	{
		return get(schema.getColumn(columnName));
	}
	
	public Object get(int columnIndex)
	{
		return get(schema.getColumn(columnIndex));
	}
	
	public Object get(Column column)
	{
		return column.retrieveValue(this);
	}

	/**
	 * @return the schema
	 */
	public Schema getSchema()
	{
		return schema;
	}
	
	public void writeToBitStream(BitOutputStream bitStream, Set<Column> skipColumns) throws Exception
	{
		try
		{
			//write fields:
			for(Column c : schema.getColumns())
				if(!skipColumns.contains(c))
					c.retrieveAndWriteValue(this, bitStream);
		}
		catch(Exception e)
		{
			throw new Exception("Error on attempting to write record", e);
		}
	}
	
	public void readFromBitStream(BitInputStream bitStream, Set<Column> skipColumns) throws Exception
	{
		try
		{
			//read fields:
			for(Column c : schema.getColumns())
				if(!skipColumns.contains(c))
					c.readAndStoreValue(this, bitStream);
		}
		catch(Exception e)
		{
			throw new Exception("Error on attempting to read record", e);
		}
	}

}
