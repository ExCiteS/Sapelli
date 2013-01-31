package uk.ac.ucl.excites.storage.model;

import uk.ac.ucl.excites.storage.io.BitOutputStream;

/**
 * @author mstevens
 *
 */
@SuppressWarnings("rawtypes")
public class Record
{
	
	static public final int UNKNOWN_ID = -1; 
	
	private Schema schema;
	private int id;
	private long deviceID;
	
	//TODO do we need record id's at all?
	
	public Record(Schema schema)
	{
		this(schema, UNKNOWN_ID, UNKNOWN_ID);
	}
	
	public Record(Schema schema, int deviceID)
	{
		this(schema, UNKNOWN_ID, deviceID);
	}
	
	public Record(Schema schema, int id, long deviceID)
	{
		if(!schema.isSealed())
			throw new IllegalArgumentException("Schema must be sealed before records based on it can be created!");
		this.schema = schema;
		this.id = id;
		this.deviceID = deviceID;
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

	/**
	 * @return the id
	 */
	public int getID()
	{
		return id;
	}

	/**
	 * @return the deviceID
	 */
	public long getDeviceID()
	{
		return deviceID;
	}
	
	public void writeToBitStream(BitOutputStream bitStream, boolean writeID, boolean writeDeviceID) throws Exception
	{
		try
		{
			if(writeID)
				bitStream.write(id); //writes ID as a 32bit integer
			if(writeDeviceID)
				bitStream.write(deviceID); //writes device ID as a 64bit integer
			//write fields:
			for(Column c : schema.getColumns())
				c.retrieveAndWriteValue(this, bitStream);
		}
		catch(Exception e)
		{
			throw new Exception("Error on attempting to write record", e);
		}
	}
	
//TODO	public void readFromBitStream(

}
