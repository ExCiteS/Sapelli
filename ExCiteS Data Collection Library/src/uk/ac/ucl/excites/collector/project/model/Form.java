/**
 *
 */
package uk.ac.ucl.excites.collector.project.model;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.collector.project.model.fields.EndField;
import uk.ac.ucl.excites.collector.project.model.fields.Field;
import uk.ac.ucl.excites.collector.project.model.fields.LocationField;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.DateTimeColumn;
import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.util.CollectionUtils;

/**
 * @author mstevens, Michalis Vitos
 *
 */
public class Form
{

	// Statics--------------------------------------------------------
	/**
	 * Allowed form indexes: 0 to {@link Project#MAX_FORMS} - 1
	 */
	public static final IntegerRangeMapping FORM_INDEX_FIELD = new IntegerRangeMapping(0, Project.MAX_FORMS - 1);
	public static final int FORM_INDEX_SIZE = FORM_INDEX_FIELD.getSize(); //bits
	
	public static final boolean END_TIME_DEFAULT = false;

	// Where to go next:
	static public enum Next
	{
		LOOPFORM,
		EXITAPP,
		PREVFORM
	}
	public static final Next DEFAULT_NEXT = Next.LOOPFORM;
	public static final String V1X_NEXT_LOOP = "_LOOP";
	public static final String V1X_NEXT_EXIT = "_EXIT";
	
	public static final boolean DEFAULT_SINGLE_PAGE = false;
	public static final boolean DEFAULT_VIBRATE = true;
	public static final boolean DEFAULT_SHOW_BACK = true;
	public static final boolean DEFAULT_SHOW_CANCEL = true;
	public static final boolean DEFAULT_SHOW_FORWARD = true;
	public static final String DEFAULT_BUTTON_BACKGROUND_COLOR = "#E8E8E8"; //light gray
	public static final boolean DEFAULT_ANIMATION = true;

	public static final String COLUMN_TIMESTAMP_START = "StartTime";
	public static final String COLUMN_TIMESTAMP_END = "EndTime";
	public static final String COLUMN_DEVICE_ID = "DeviceID";
	// public static final String COLUMN_USER = "User";
	public static final String COLUMN_SENT_AT = "SentAt";
	public static final String COLUMN_RECEIVED_AT = "ReceivedAt";
	public static final String COLUMN_TRANSMISSION_TYPE = "TransmissionType";

	// Dynamics-------------------------------------------------------
	private final Project project;
	private final int index;
	private Schema schema;
	private final String id;

	private transient List<String> warnings;
	
	// Fields
	private Field start;
	private final List<Field> fields;
	private final List<LocationField> locationFields;
	
	// Android shortcut:
	private String shortcutImageRelativePath;

	// Animation:
	private boolean animation = DEFAULT_ANIMATION;
	
	// Timestamps
	private boolean storeEndTime;

	// End action:
	private Next next = DEFAULT_NEXT;
	private boolean vibrateOnSave = DEFAULT_VIBRATE;
	private String saveSoundRelativePath;

	// Buttons:
	private boolean showBack = DEFAULT_SHOW_BACK;
	private boolean showCancel = DEFAULT_SHOW_CANCEL;
	private boolean showForward = DEFAULT_SHOW_FORWARD;
	private String buttonBackgroundColor = DEFAULT_BUTTON_BACKGROUND_COLOR;
	private String backButtonImageRelativePath;
	private String cancelButtonImageRelativePath;
	private String forwardButtonImageRelativePath;
	
	public Form(Project project, String id)
	{
		this.project = project;
		this.id = id;
		
		this.fields = new ArrayList<Field>();
		this.locationFields = new ArrayList<LocationField>();
		
		// Set Form index & add it to the Project:
		if(FORM_INDEX_FIELD.fits(project.getForms().size()))
			this.index = project.getForms().size();
		else
			throw new IllegalArgumentException("Invalid form index, valid values are " + FORM_INDEX_FIELD.getLogicalRangeString() + " (up to " + Project.MAX_FORMS + " forms per project).");
		project.addForm(this); //!!!
	}

	public void initialiseStorage()
	{
		getSchema(); //this will also trigger all Columns to be created/initialised
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
				next = new EndField(this, true); // current field is the last of the form, go to default EndField
		return next; // use jump as next
	}

	/**
	 * Returns the Form ID
	 * 
	 * @return
	 */
	public String getID()
	{
		return id;
	}
	
	/**
	 * Returns the Form ID (method kept for backwards compatibility)
	 * 
	 * @return
	 */
	public String getName()
	{
		return getID();
	}

	public List<Field> getFields()
	{
		return fields;
	}
	
	/**
	 * Find a field of the form by its ID 
	 * 
	 * @param fieldID
	 * @return the field with the specified ID, or null if no such field exists in this form
	 */
	public Field getField(String fieldID)
	{
		for(Field f : fields)
			if(f.getID().equals(fieldID))
				return f;
		return null;
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
	 * @return the animation
	 */
	public boolean isAnimation()
	{
		return animation;
	}

	/**
	 * @param animation the animation to set
	 */
	public void setAnimation(boolean animation)
	{
		this.animation = animation;
	}

	/**
	 * @return the shortcutImageRelativePath
	 */
	public String getShortcutImageRelativePath()
	{
		return shortcutImageRelativePath;
	}

	/**
	 * @param shortcutImageRelativePath
	 *            the shortcutImageRelativePath to set
	 */
	public void setShortcutImageRelativePath(String shortcutImageRelativePath)
	{
		this.shortcutImageRelativePath = shortcutImageRelativePath;
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
	 * @return the backButtonImageRelativePath
	 */
	public String getBackButtonImageRelativePath()
	{
		return backButtonImageRelativePath;
	}

	/**
	 * @param backButtonImageRelativePath the backButtonImageRelativePath to set
	 */
	public void setBackButtonImageRelativePath(String backButtonImageRelativePath)
	{
		this.backButtonImageRelativePath = backButtonImageRelativePath;
	}

	/**
	 * @return the cancelButtonImageRelativePath
	 */
	public String getCancelButtonImageRelativePath()
	{
		return cancelButtonImageRelativePath;
	}

	/**
	 * @param cancelButtonImageRelativePath the cancelButtonImageRelativePath to set
	 */
	public void setCancelButtonImageRelativePath(String cancelButtonImageRelativePath)
	{
		this.cancelButtonImageRelativePath = cancelButtonImageRelativePath;
	}

	/**
	 * @return the forwardButtonImageRelativePath
	 */
	public String getForwardButtonImageRelativePath()
	{
		return forwardButtonImageRelativePath;
	}

	/**
	 * @param forwardButtonImageRelativePath the forwardButtonImageRelativePath to set
	 */
	public void setForwardButtonImageRelativePath(String forwardButtonImageRelativePath)
	{
		this.forwardButtonImageRelativePath = forwardButtonImageRelativePath;
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
	 * @return the next
	 */
	public Next getNext()
	{
		return next;
	}

	/**
	 * @param next the next to set
	 * @throws IllegalArgumentException	when the nextStr is not recognised
	 */
	public void setNext(String nextStr) throws IllegalArgumentException
	{
		try
		{
			this.next = Next.valueOf(nextStr);
		}
		catch(IllegalArgumentException iae)
		{
			if(V1X_NEXT_LOOP.equalsIgnoreCase(nextStr))
				this.next = Next.LOOPFORM;
			else if(V1X_NEXT_EXIT.equalsIgnoreCase(nextStr))
				this.next = Next.EXITAPP;
			else
				throw iae;
		}
	}

	/**
	 * @return the vibrateOnSave
	 */
	public boolean isVibrateOnSave()
	{
		return vibrateOnSave;
	}

	/**
	 * @param vibrateOnSave
	 *
	 */
	public void setVibrateOnSave(boolean vibrateOnSave)
	{
		this.vibrateOnSave = vibrateOnSave;
	}

	/**
	 * @return the saveSoundRelativePath
	 */
	public String getSaveSoundRelativePath()
	{
		return saveSoundRelativePath;
	}

	/**
	 * Set the save sound
	 * @param saveSoundRelativePath
	 */
	public void setSaveSoundRelativePath(String saveSoundRelativePath)
	{
		this.saveSoundRelativePath = saveSoundRelativePath;
	}

	/**
	 * @return the index
	 */
	public int getIndex()
	{
		return index;
	}

	public Schema getSchema()
	{
		if(schema == null)
		{
			//create new one:
			schema = new Schema(project.getHash(), /* Project#hash becomes Schema#usageID */
								this.index, /* Form#index becomes Schema#usageSubID */
								project.getName() +
								(project.getVariant() != null ? '_' + project.getVariant() : "") +
								"_v" + project.getVersion() +
								":" + id /* = form "name"*/);
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
					schema.addColumns(f.getColumns());
			// Seal & store the schema:
			schema.seal();
		}
		return schema;
	}
	
	/**
	 * Override the schema object with another one, if compatible
	 * 
	 * @param newSchema
	 */
	/*public void setSchema(Schema newSchema)
	{
		if(getSchema().equals(newSchema, true, true)) // check if the schema is identical/equivalent to the one we have/need 
			this.schema = newSchema; // we accept the new one
		else
			throw new IllegalArgumentException("The provived schema is not compatible with this form!");
	}*/

	public Record newEntry(long deviceID)
	{
		Record record = new Record(getSchema()); //call getSchema() once to make sure the schema (and columns) are initialised

		// Set current time as start timestamp
		((DateTimeColumn) schema.getColumn(COLUMN_TIMESTAMP_START)).storeValue(record, new DateTime() /*= now*/);

		// Set deviceID
		((IntegerColumn) schema.getColumn(COLUMN_DEVICE_ID)).storeValue(record, deviceID);

		return record;
	}
	
	public Column<?> getColumnFor(Field field)
	{
		return getSchema().getColumn(field.getID());
	}
	
	public void finish(Record record)
	{
		if(storeEndTime)
			// Set current time as end timestamp
			((DateTimeColumn) schema.getColumn(COLUMN_TIMESTAMP_END)).storeValue(record, new DateTime() /*= now*/);
	}
	
	public void addWarning(String warning)
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
	
	public List<File> getFiles(Project project)
	{
		List<File> paths = new ArrayList<File>();
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(backButtonImageRelativePath));
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(cancelButtonImageRelativePath));
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(forwardButtonImageRelativePath));
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(shortcutImageRelativePath));
		CollectionUtils.addIgnoreNull(paths, project.getSoundFile(saveSoundRelativePath));
		//Add paths for fields:
		for(Field field : fields)
			CollectionUtils.addAllIgnoreNull(paths, field.getFiles(project));
		return paths;
	}
	
	public String toString()
	{
		return id;
	}

}
