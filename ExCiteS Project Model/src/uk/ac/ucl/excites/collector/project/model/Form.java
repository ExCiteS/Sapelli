/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.db.DataAccess;

import uk.ac.ucl.excites.storage.model.DateTimeColumn;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class Form
{
	
	//Statics--------------------------------------------------------
	public static final int END_ACTION_LOOP = 0;
	public static final int END_ACTION_EXIT = 1;
	//public static final int END_ACTION_NEXT = 2;
	public static final int END_ACTION_DEFAULT = END_ACTION_LOOP;
	
	public static final String COLUMN_TIMESTAMP = "TimeStamp";
	public static final String COLUMN_DEVICE_ID = "DeviceID";
	//public static final String COLUMN_USER = "User";
	public static final String COLUMN_SENT_AT = "SentAt";
	public static final String COLUMN_RECEIVED_AT = "ReceivedAt";
	public static final String COLUMN_TRANSMISSION_TYPE = "TransmissionType";
	
	//Dynamics-------------------------------------------------------
	private int schemaID;
	private int schemaVersion;
	private String name;
	
	//Fields
	private Field start;
	private List<Field> fields;
	private LocationField locationField;

	//Android shortcut:
	private boolean shortcut;
	//shortcutIcon 

	//End action:
	private int endAction;
	private boolean vibrateOnEnd;
	private String endSoundPath;
	
	//Buttons:
	private boolean showBack;
	private boolean showHome;
	
	public Form(String name, int schemaID)
	{
		this(name, schemaID, Schema.DEFAULT_VERSION);
	}
	
	public Form(String name, int schemaID, int schemaVersion)
	{
		this.name = name;
		this.schemaID = schemaID;
		this.schemaVersion = schemaVersion;
		this.fields = new ArrayList<Field>();
	}	
	
	public void addField(Field f)
	{
		fields.add(f);
		if(f instanceof LocationField)
		{
			if(locationField != null)
				throw new IllegalStateException("For now we only support 1 Location field per Form (this may change in the future).");
			locationField = (LocationField) f;
		}
	}
	
	public Field getNextField(Field current)
	{
		int currentIndex = fields.indexOf(current);
		//Exception handling:
		if(currentIndex < 0)
			throw new IllegalArgumentException("The current field is not part of this form.");
		//Check for jump field (possibly the one of a parent in case of Choice):
		Field jump = current.getJump();
		if(jump != null)
			return jump; //use jump as next
		//No jump is set, check for field below current one
		if(currentIndex + 1 < fields.size())
			return fields.get(currentIndex + 1); //go to next field in the form
		else
			return EndField.getInstance(); //current field is the last of the form, go to end
	}
	
	public String getName()
	{
		return name;
	}
	
	public List<Field> getFields()
	{
		return fields;
	}

	/**
	 * @return the start
	 */
	public Field getStart()
	{
		return start;
	}

	/**
	 * @param start the start to set
	 */
	public void setStart(Field start)
	{
		this.start = start;
	}
	
	public boolean hasLocationField()
	{
		return locationField != null; 
	}
	
	public LocationField getLocationField()
	{
		return locationField;
	}
	
	/**
	 * @return the endAction
	 */
	public int getEndAction()
	{
		return endAction;
	}

	/**
	 * @return the vibrateOnEnd
	 */
	public boolean isVibrateOnEnd()
	{
		return vibrateOnEnd;
	}

	/**
	 * @return the endSoundPath
	 */
	public String getEndSoundPath()
	{
		return endSoundPath;
	}

	public Schema getSchema(DataAccess dao)
	{	
		Schema schema = dao.retrieveSchema(schemaID, schemaVersion);
		if(schema == null)
		{
			schema = new Schema(schemaID, schemaVersion, name);
			//Internal-use columns:
			// Device ID column:
			
			// Timestamp column:
			schema.addColumn(DateTimeColumn.Century21NoMS(COLUMN_TIMESTAMP, false));
			// Transmission information columns:
			
			
			
			//Columns for user-defined fields:
			for(Field f : fields)
				if(!f.isNoColumn())
					schema.addColumn(f.createColumn());
			//Seal & store the schema:
			schema.seal();
			dao.store(schema); //!!!
		}
		return schema;
	}
	
	public FormEntry newEntry(DataAccess dao)
	{
		FormEntry entry = new FormEntry(this, dao);
		
		//TODO set current time as timestamp
		
		//TODO set deviceID
		
		return entry;
	}
	
}
