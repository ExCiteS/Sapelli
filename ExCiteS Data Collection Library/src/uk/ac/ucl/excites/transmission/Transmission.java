/**
 * 
 */
package uk.ac.ucl.excites.transmission;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 * 
 */
public abstract class Transmission
{

	protected DateTime sentAt = null; //used only on sending side
	protected DateTime receivedAt = null; //used on receiving side, and TODO on sending side once we have acknowledgements working
	
	protected Settings settings;
	protected ModelProvider modelProvider; //only used on the receiving side
	protected Schema schema;
	protected Set<Column<?>> columnsToFactorOut;
	protected Map<Column<?>, Object> factoredOutValues = null;
	protected List<Record> records;

	/**
	 * To be called at the sending side.
	 * 
	 * @param schema
	 * @param settings
	 */
	public Transmission(Schema schema, Settings settings)
	{
		this(schema, null, settings);
	}
	
	/**
	 * To be called at the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param settings
	 */
	public Transmission(Schema schema, Set<Column<?>> columnsToFactorOut, Settings settings)
	{
		this(); //!!!
		if(schema == null)
			throw new NullPointerException("Schema cannot be null on sending side.");
		this.schema = schema;
		setColumnsToFactorOut(columnsToFactorOut);
		this.settings = settings;
	}
	
	/**
	 * To be called at the receiving side.
	 * 
	 * @param modelProvider
	 * @param settings
	 */
	public Transmission(ModelProvider modelProvider)
	{
		this(); //!!!
		if(modelProvider == null)
			throw new NullPointerException("ModelProvider cannot be null on receiving side.");
		this.modelProvider = modelProvider;
	}
	
	private Transmission()
	{
		this.factoredOutValues = new HashMap<Column<?>, Object>();
		this.records = new ArrayList<Record>();
	}
	
	protected void setColumnsToFactorOut(Set<Column<?>> columnsToFactorOut)
	{
		if(columnsToFactorOut == null)
			columnsToFactorOut = new HashSet<Column<?>>();
		else
		{
			for(Column<?> c : columnsToFactorOut)
				if(schema.getColumnIndex(c) == Schema.UNKNOWN_COLUMN_INDEX)
					throw new IllegalArgumentException(c.toString() + " does not belong to the given schema.");
		}
		this.columnsToFactorOut = columnsToFactorOut;
	}
	
	public abstract boolean addRecord(Record record) throws Exception;
	
	public List<Record> getRecords()
	{
		return records;
	}
	
	public abstract void send(TransmissionSender sender) throws Exception;
	
	public void resend(TransmissionSender sender) throws Exception
	{
		//Clear early sentAt value (otherwise send() won't work):
		sentAt = null;
		
		//Resend:
		send(sender);
	}
	
	public abstract void receive() throws Exception;
	
	public abstract boolean isFull();
	
	public boolean isEmpty()
	{
		return records.isEmpty();
	}
	
	public boolean isSent()
	{
		return sentAt != null;
	}

	public boolean isReceived()
	{
		return receivedAt != null;
	}
	
}
