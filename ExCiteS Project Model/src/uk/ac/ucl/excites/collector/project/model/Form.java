/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.List;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.storage.model.DateTimeColumn;
import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class Form
{
	
	//Statics--------------------------------------------------------
	public static final boolean END_TIME_DEFAULT = false;
	
	public static final int END_ACTION_LOOP = 0;
	public static final int END_ACTION_EXIT = 1;
	//public static final int END_ACTION_NEXT_FORM = 2;
	public static final int DEFAULT_END_ACTION = END_ACTION_LOOP;
	
	public static boolean DEFAULT_SHOW_BACK = true;
	public static boolean DEFAULT_SHOW_CANCEL = true;
	public static boolean DEFAULT_SHOW_FORWARD = false; //for now we never show the forward button
	public static String DEFAULT_BUTTON_BACKGROUND_COLOR = "#FFFFFF"; //White
	
	public static final String COLUMN_TIMESTAMP_START = "StartTime";
	public static final String COLUMN_TIMESTAMP_END = "EndTime";
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
	private List<LocationField> locationFields;

	//Android shortcut:
	private boolean shortcut;
	//shortcutIcon 

	//Timestamps
	private boolean storeEndTime;
	
	//End action:
	private int endAction;
	private boolean vibrateOnEnd;
	private String endSoundPath;
	
	//Buttons:
	private boolean showBack;
	private boolean showCancel;
	private boolean showForward;
	private String backgroundColor;
	
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
		this.locationFields = new ArrayList<LocationField>();
		this.endAction = DEFAULT_END_ACTION;
	}
	
	public void addField(Field f)
	{
		fields.add(f);
		if(f instanceof LocationField)
			locationFields.add((LocationField) f);
	}
	
	public Field getNextField(Field current)
	{
		int currentIndex = fields.indexOf(current.getRoot());
		//Exception handling:
		if(currentIndex < 0)
			throw new IllegalArgumentException("The current field is not part of this form.");
		//Check for jump field (possibly the one of a parent in case of Choice):
		Field next = current.getJump();
		if(next == null)
		{
			//No jump is set, check for field below current one:
			if(currentIndex + 1 < fields.size())
				next = fields.get(currentIndex + 1); //go to next field in the form
			else
				next = new EndField(); //current field is the last of the form, go to end
		}
		return next; //use jump as next
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
	
	/**
	 * @return the showBack
	 */
	public boolean isShowBack()
	{
		return showBack;
	}

	/**
	 * @param showBack the showBack to set
	 */
	public void setShowBack(boolean showBack)
	{
		this.showBack = showBack;
	}

	/**
	 * @return the showCancel
	 */
	public boolean isShowCancel()
	{
		return showCancel;
	}

	/**
	 * @param showCancel the showCancel to set
	 */
	public void setShowCancel(boolean showCancel)
	{
		this.showCancel = showCancel;
	}

	/**
	 * @return the showForward
	 */
	public boolean isShowForward()
	{
		return showForward;
	}

	/**
	 * @param showForward the showForward to set
	 */
	public void setShowForward(boolean showForward)
	{
		this.showForward = showForward;
	}

	public List<LocationField> getLocationFields()
	{
		return locationFields;
	}
	
	public List<LocationField> getLocationFields(boolean onlyStartWithForm)
	{
		if(onlyStartWithForm)
		{
			List<LocationField> startLF = new ArrayList<LocationField>();
			for(LocationField lf : getLocationFields())
				if(lf.isStartWithForm())
					startLF.add(lf);
			return startLF;
		}
		return getLocationFields();
	}
	
	/**
	 * @return the storeEndTime
	 */
	public boolean isStoreEndTime()
	{
		return storeEndTime;
	}

	/**
	 * @param storeEndTime the storeEndTime to set
	 */
	public void setStoreEndTime(boolean storeEndTime)
	{
		this.storeEndTime = storeEndTime;
	}

	/**
	 * @return the endAction
	 */
	public int getEndAction()
	{
		return endAction;
	}
		
	/**
	 * @param endAction the endAction to set
	 */
	public void setEndAction(int endAction)
	{
		this.endAction = endAction;
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
	
	public String getBackImagePath()
	{
		return null; //TODO
	}
	
	public String getCancelImagePath()
	{
		return null; //TODO
	}
	
	public String getForwardImagePath()
	{
		return null; //TODO
	}

	public Schema getSchema(DataAccess dao)
	{	
		Schema schema = dao.retrieveSchema(schemaID, schemaVersion);
		if(schema == null)
		{
			schema = new Schema(schemaID, schemaVersion, name);
			//Internal-use columns:
			// Timestamp column(s):
			schema.addColumn(DateTimeColumn.Century21NoMS(COLUMN_TIMESTAMP_START, false));
			if(storeEndTime)
				schema.addColumn(DateTimeColumn.Century21NoMS(COLUMN_TIMESTAMP_END, false));			
			// Device ID column:
			schema.addColumn(new IntegerColumn(COLUMN_DEVICE_ID, false, false, 32));
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
	
	public FormEntry newEntry(DataAccess dao, long deviceID)
	{
		FormEntry entry = new FormEntry(this, dao);
		
		//TODO Set current time as start timestamp
		DateTime dt;
		
		//TODO Set deviceID
		
		
		return entry;
	}
	
}
