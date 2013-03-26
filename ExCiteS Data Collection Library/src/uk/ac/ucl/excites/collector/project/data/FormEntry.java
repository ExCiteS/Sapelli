/**
 * 
 */
package uk.ac.ucl.excites.collector.project.data;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.storage.model.DateTimeColumn;
import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * Simple wrapper around Record to make Form-specific columns/values more easily accessible
 * 
 * @author mstevens
 *
 */
public class FormEntry
{

	private Form form;
	private Record record;
	private Schema schema;
	
	public FormEntry(Form form, Record record)
	{
		if(!record.getSchema().equals(form.getSchema()))
			throw new IllegalArgumentException("Schema mismatch!");
		this.form = form;
		this.record = record;
		this.schema = form.getSchema();
	}
	
	public DateTime getStartTime()
	{
		return getStartTime(false);
	}
	
	public DateTime getStartTime(boolean asStoredBinary)
	{
		DateTimeColumn dtCol = (DateTimeColumn) schema.getColumn(Form.COLUMN_TIMESTAMP_START);
		if(asStoredBinary)
			return dtCol.retrieveValueAsStoredBinary(record);
		else
			return dtCol.retrieveValue(record);
	}
	
	public DateTime getEndTime()
	{
		if(form.isStoreEndTime())
			return ((DateTimeColumn) schema.getColumn(Form.COLUMN_TIMESTAMP_END)).retrieveValue(record);
		else
			return null;
	}
	
	public long getDeviceID()
	{
		return ((IntegerColumn) schema.getColumn(Form.COLUMN_DEVICE_ID)).retrieveValue(record);
	}
	
}
