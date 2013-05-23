/**
 *
 */
package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.List;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.model.DateTimeColumn;
import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens, Michalis Vitos
 *
 */
public class Form
{

	// Statics--------------------------------------------------------
	public static final boolean END_TIME_DEFAULT = false;

	public static final int END_ACTION_LOOP = 0;
	public static final int END_ACTION_EXIT = 1;
	// public static final int END_ACTION_NEXT_FORM = 2;
	public static final int DEFAULT_END_ACTION = END_ACTION_LOOP;

	public static final boolean DEFAULT_VIBRATE = true;
	public static final boolean DEFAULT_SHOW_BACK = true;
	public static final boolean DEFAULT_SHOW_CANCEL = true;
	public static final boolean DEFAULT_SHOW_FORWARD = true;
	public static final String DEFAULT_BUTTON_BACKGROUND_COLOR = "#E8E8E8"; //light gray

	public static final String COLUMN_TIMESTAMP_START = "StartTime";
	public static final String COLUMN_TIMESTAMP_END = "EndTime";
	public static final String COLUMN_DEVICE_ID = "DeviceID";
	// public static final String COLUMN_USER = "User";
	public static final String COLUMN_SENT_AT = "SentAt";
	public static final String COLUMN_RECEIVED_AT = "ReceivedAt";
	public static final String COLUMN_TRANSMISSION_TYPE = "TransmissionType";

	// Dynamics-------------------------------------------------------
	private final Project project;
	private final int schemaID;
	private final int schemaVersion;
	private Schema schema;
	private final String name;

	private transient List<String> warnings;
	
	// Fields
	private Field start;
	private final List<Field> fields;
	private final List<LocationField> locationFields;

	// Android shortcut:
	private String shortcutImageLogicalPath;

	// Timestamps
	private boolean storeEndTime;

	// End action:
	private int endAction;
	private boolean vibrateOnEnd;
	private String endSoundPath;

	// Buttons:
	private boolean showBack = DEFAULT_SHOW_BACK;
	private boolean showCancel = DEFAULT_SHOW_CANCEL;
	private boolean showForward = DEFAULT_SHOW_FORWARD;
	private String backButtonImageLogicalPath;
	private String cancelButtonImageLogicalPath;
	private String forwardButtonImageLogicalPath;
	private String buttonBackgroundColor;

	public Form(Project project, String name, int schemaID)
	{
		this(project, name, schemaID, Schema.DEFAULT_VERSION);
	}

	public Form(Project project, String name, int schemaID, int schemaVersion)
	{
		this.project = project;
		this.name = name;
		if(Schema.SCHEMA_ID_FIELD.fits(schemaID))
			this.schemaID = schemaID;
		else
			throw new IllegalArgumentException("Invalid schema ID, valid values are " + Schema.SCHEMA_ID_FIELD.getLogicalRangeString() + ".");
		if(Schema.SCHEMA_VERSION_FIELD.fits(schemaVersion))
			this.schemaVersion = schemaVersion;
		else
			throw new IllegalArgumentException("Invalid schema version, valid values are " + Schema.SCHEMA_VERSION_FIELD.getLogicalRangeString() + ".");
		this.fields = new ArrayList<Field>();
		this.locationFields = new ArrayList<LocationField>();
		this.buttonBackgroundColor = DEFAULT_BUTTON_BACKGROUND_COLOR;
		this.endAction = DEFAULT_END_ACTION;
		this.vibrateOnEnd = DEFAULT_VIBRATE;
	}

	public void initialiseStorage()
	{
		getSchema(); //this will also trigger all Column and ValueDictionaries to be created/initialised
	}

	/**
	 * @return the project
	 */
	public Project getProject()
	{
		return project;
	}

	public void addField(Field f)
	{
		fields.add(f);
		if(f instanceof LocationField)
			locationFields.add((LocationField) f);
	}

	public int getFieldIndex(Field field)
	{
		return fields.indexOf(field.getRoot());
	}

	public Field getNextField(Field current)
	{
		int currentIndex = getFieldIndex(current);
		// Exception handling:
		if(currentIndex < 0)
			throw new IllegalArgumentException("The current field is not part of this form.");
		// Check for jump field (possibly the one of a parent in case of ChoiceField):
		Field next = current.getJump();
		if(next == null)
			// No jump is set, check for field below current one:
			if(currentIndex + 1 < fields.size())
				next = fields.get(currentIndex + 1); // go to next field in the form
			else
				next = new EndField(this); // current field is the last of the form, go to end
		return next; // use jump as next
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
	public Field getStartField()
	{
		return start;
	}

	/**
	 * @param start
	 *            the start to set
	 */
	public void setStartField(Field start)
	{
		this.start = start;
	}

	/**
	 * @return the shortcutImageLogicalPath
	 */
	public String getShortcutImageLogicalPath()
	{
		return shortcutImageLogicalPath;
	}

	/**
	 * @param shortcutImageLogicalPath
	 *            the shortcutImageLogicalPath to set
	 */
	public void setShortcutImageLogicalPath(String shortcutImageLogicalPath)
	{
		this.shortcutImageLogicalPath = shortcutImageLogicalPath;
	}

	/**
	 * @return the showBack
	 */
	public boolean isShowBack()
	{
		return showBack;
	}

	/**
	 * @param showBack
	 *            the showBack to set
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
	 * @param showCancel
	 *            the showCancel to set
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
	 * @param showForward
	 *            the showForward to set
	 */
	public void setShowForward(boolean showForward)
	{
		this.showForward = showForward;
	}

	/**
	 * @return the backButtonImageLogicalPath
	 */
	public String getBackButtonImageLogicalPath()
	{
		return backButtonImageLogicalPath;
	}

	/**
	 * @param backButtonImageLogicalPath the backButtonImageLogicalPath to set
	 */
	public void setBackButtonImageLogicalPath(String backButtonImageLogicalPath)
	{
		this.backButtonImageLogicalPath = backButtonImageLogicalPath;
	}

	/**
	 * @return the cancelButtonImageLogicalPath
	 */
	public String getCancelButtonImageLogicalPath()
	{
		return cancelButtonImageLogicalPath;
	}

	/**
	 * @param cancelButtonImageLogicalPath the cancelButtonImageLogicalPath to set
	 */
	public void setCancelButtonImageLogicalPath(String cancelButtonImageLogicalPath)
	{
		this.cancelButtonImageLogicalPath = cancelButtonImageLogicalPath;
	}

	/**
	 * @return the forwardButtonImageLogicalPath
	 */
	public String getForwardButtonImageLogicalPath()
	{
		return forwardButtonImageLogicalPath;
	}

	/**
	 * @param forwardButtonImageLogicalPath the forwardButtonImageLogicalPath to set
	 */
	public void setForwardButtonImageLogicalPath(String forwardButtonImageLogicalPath)
	{
		this.forwardButtonImageLogicalPath = forwardButtonImageLogicalPath;
	}

	/**
	 * @return the buttonBackgroundColor
	 */
	public String getButtonBackgroundColor()
	{
		return buttonBackgroundColor;
	}

	/**
	 * @param buttonBackgroundColor the buttonBackgroundColor to set
	 */
	public void setButtonBackgroundColor(String buttonBackgroundColor)
	{
		this.buttonBackgroundColor = buttonBackgroundColor;
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
	 * @param storeEndTime
	 *            the storeEndTime to set
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
	 * @param endAction
	 *            the endAction to set
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
	 * @param vibrateOnEnd
	 *
	 */
	public void setVibrateOnEnd(boolean vibrateOnEnd)
	{
		this.vibrateOnEnd = vibrateOnEnd;
	}

	/**
	 * @return the endSoundPath
	 */
	public String getEndSoundPath()
	{
		return endSoundPath;
	}

	/**
	 * Set the end sound
	 * @param endSoundPath
	 */
	public void setEndSoundPath(String endSoundPath)
	{
		this.endSoundPath = endSoundPath;
	}

	/**
	 * @return the schemaID
	 */
	public int getSchemaID()
	{
		return schemaID;
	}

	/**
	 * @return the schemaVersion
	 */
	public int getSchemaVersion()
	{
		return schemaVersion;
	}

	public Schema getSchema()
	{
		if(schema == null)
		{
			//create new one:
			schema = new Schema(schemaID, schemaVersion, name);
			// Internal-use columns:
			// Timestamp column(s):
			schema.addColumn(DateTimeColumn.Century21NoMS(COLUMN_TIMESTAMP_START, false));
			if(storeEndTime)
				schema.addColumn(DateTimeColumn.Century21NoMS(COLUMN_TIMESTAMP_END, false));
			// Device ID column:
			schema.addColumn(new IntegerColumn(COLUMN_DEVICE_ID, false, false, 32));
			// Columns for user-defined fields:
			for(Field f : fields)
				if(!f.isNoColumn())
					schema.addColumn(f.getColumn());
			// Seal & store the schema:
			schema.seal();
		}
		return schema;
	}

	public Record newEntry(long deviceID)
	{
		Record record = new Record(getSchema()); //call getSchema() once to make sure the schema (and columns) are initialised

		// Set current time as start timestamp
		((DateTimeColumn) schema.getColumn(COLUMN_TIMESTAMP_START)).storeValue(record, new DateTime() /*= now*/);

		// Set deviceID
		((IntegerColumn) schema.getColumn(COLUMN_DEVICE_ID)).storeValue(record, deviceID);

		return record;
	}

	public void finish(Record record)
	{
		if(storeEndTime)
			// Set current time as end timestamp
			((DateTimeColumn) schema.getColumn(COLUMN_TIMESTAMP_END)).storeValue(record, new DateTime() /*= now*/);
	}
	
	protected void addWarning(String warning)
	{
		if(warnings == null)
			warnings = new ArrayList<String>();
		warnings.add(warning);
	}
	
	public List<String> getWarnings()
	{
		if(warnings == null)
			return new ArrayList<String>(); //leave this.warnings null
		return warnings;
	}

}
