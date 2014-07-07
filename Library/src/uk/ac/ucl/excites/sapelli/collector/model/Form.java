/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.model;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.SapelliCollectorClient;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Index;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;

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
	public static final int FORM_POSITION_SIZE = Schema.SCHEMA_ID_SIZE /* 36 */ - Project.PROJECT_HASH_SIZE /* 32 */; // = 4 bits
	public static final IntegerRangeMapping FORM_POSITION_FIELD = IntegerRangeMapping.ForSize(0, FORM_POSITION_SIZE);
	
	public static final boolean END_TIME_DEFAULT = false;

	// Where to go next:
	static public enum Next
	{
		LOOPFORM,
		LOOPPROJ,
		PREVFORM,
		NEXTFORM,
		EXITAPP
	}
	public static final Next DEFAULT_NEXT = Next.LOOPFORM;
	public static final String V1X_NEXT_LOOP = "LOOP";
	public static final String V1X_NEXT_EXIT = "EXIT";
	
	public static final boolean V1X_DEFAULT_SHOW_BACK = true;
	public static final boolean V1X_DEFAULT_SHOW_CANCEL = true;
	public static final boolean V1X_DEFAULT_SHOW_FORWARD = true;
	
	public static final boolean DEFAULT_SKIP_ON_BACK = false;
	public static final boolean DEFAULT_SINGLE_PAGE = false;
	public static final boolean DEFAULT_VIBRATE = true;
	public static final String DEFAULT_BUTTON_BACKGROUND_COLOR = "#BABABA"; // gray
	public static final boolean DEFAULT_ANIMATION = true;
	public static final boolean DEFAULT_OBFUSCATE_MEDIA_FILES = false;

	public static final String COLUMN_TIMESTAMP_START_NAME = "StartTime";
	public static final TimeStampColumn COLUMN_TIMESTAMP_START = TimeStampColumn.Century21NoMS(COLUMN_TIMESTAMP_START_NAME, false, true);
	public static final String COLUMN_TIMESTAMP_END_NAME = "EndTime";
	public static final TimeStampColumn COLUMN_TIMESTAMP_END = TimeStampColumn.Century21NoMS(COLUMN_TIMESTAMP_END_NAME, false, true);
	public static final String COLUMN_DEVICE_ID_NAME = "DeviceID";
	public static final IntegerColumn COLUMN_DEVICE_ID = new IntegerColumn(COLUMN_DEVICE_ID_NAME, false, false, 32);
	
	// The Animation type between different Screen Pages
	public static enum PageAnimation
	{
		NONE, VERTICAL, HORIZONTAL
	}

	// PageAnimation
	public static final PageAnimation DEFAULT_PAGE_ANIMATION = PageAnimation.NONE;
	// Buttons Default Description Text (used for accessibility support)
	public static final String DEFAULT_FORWARD_BUTTON_DESCRIPTION = "Forward";
	public static final String DEFAULT_CANCEL_BUTTON_DESCRIPTION = "Cancel";
	public static final String DEFAULT_BACK_BUTTON_DESCRIPTION = "Back";

	// Dynamics-------------------------------------------------------
	private final Project project;
	private final int position;
	private boolean producesRecords = true;
	private boolean skipOnBack = DEFAULT_SKIP_ON_BACK;
	private Schema schema;
	private final String id;

	private transient List<String> warnings;
	
	// Fields
	private Field start;
	private final List<Field> fields;
	private final List<Trigger> triggers;
	
	// Android shortcut:
	private String shortcutImageRelativePath;

	// Animation:
	private boolean animation = DEFAULT_ANIMATION;
	
	// PageAnimation:
	private PageAnimation pageAnimation = DEFAULT_PAGE_ANIMATION;

	// Obfuscate Media Files:
	private boolean obfuscateMediaFiles = DEFAULT_OBFUSCATE_MEDIA_FILES;

	// Timestamps
	private boolean storeEndTime;

	// End action:
	private Next next = DEFAULT_NEXT;
	private boolean vibrateOnSave = DEFAULT_VIBRATE;
	private String saveSoundRelativePath;

	// Buttons:
	private String buttonBackgroundColor = DEFAULT_BUTTON_BACKGROUND_COLOR;
	private String backButtonImageRelativePath;
	private String backButtonDescription = DEFAULT_BACK_BUTTON_DESCRIPTION;
	private String cancelButtonImageRelativePath;
	private String cancelButtonDescription = DEFAULT_CANCEL_BUTTON_DESCRIPTION;
	private String forwardButtonImageRelativePath;
	private String forwardButtonDescription = DEFAULT_FORWARD_BUTTON_DESCRIPTION;
	
	public Form(Project project, String id)
	{
		this.project = project;
		this.id = id;
		
		this.fields = new ArrayList<Field>();
		this.triggers = new ArrayList<Trigger>();
		
		// Set Form index & add it to the Project:
		if(FORM_POSITION_FIELD.fits(project.getForms().size()))
			this.position = project.getForms().size();
		else
			throw new IllegalArgumentException("Invalid form index, valid values are " + FORM_POSITION_FIELD.getLogicalRangeString() + " (up to " + Project.MAX_FORMS + " forms per project).");
		project.addForm(this); //!!!
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
	}

	public int getFieldPosition(Field field)
	{
		return fields.indexOf(field.getRoot());
	}

	/**
	 * @param current
	 * @return the next field to go to along with passed arguments, or null if the next field could not be determined (likely because the current field is part of a page)
	 */
	public FieldWithArguments getNextFieldAndArguments(Field current)
	{
		// Check for jump field (possibly the one of a parent in case of ChoiceField):
		Field nextF = current.getJump();
		if(nextF == null)
		{	// No jump is set, check for field below current one:
			int currentPos = getFieldPosition(current);
			if(currentPos < 0)
				// This field is not part of the form (it is likely part of a page):
				return null; // don't throw an exception here
			if(currentPos + 1 < fields.size())
				nextF = fields.get(currentPos + 1); // go to next field in the form
			else
				nextF = new EndField(this, true, next); // current field is the last of the form, go to the form's "next", but save the record first
		}
		return new FieldWithArguments(nextF, current.getNextFieldArguments());
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
			if(f.getID().equalsIgnoreCase(fieldID)) // field IDs are treated as case insensitive
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

	public void addTrigger(Trigger trigger)
	{
		triggers.add(trigger);
	}

	/**
	 * @return the triggers
	 */
	public List<Trigger> getTriggers()
	{
		return triggers;
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
	 * @return the pageAnimation
	 */
	public PageAnimation getPageAnimation()
	{
		return pageAnimation;
	}

	/**
	 * @param pageAnimationStr
	 *            the pageAnimation to set
	 */
	public void setPageAnimation(String pageAnimationStr)
	{
		if(pageAnimationStr == null)
			return; // default pageAnimation will be used
		pageAnimationStr = pageAnimationStr.toUpperCase(); // Make upper case
		try
		{
			this.pageAnimation = PageAnimation.valueOf(pageAnimationStr);
		}
		catch(IllegalArgumentException iae)
		{
			throw iae;
		}
	}

	/**
	 * @return the obfuscateMediaFiles
	 */
	public boolean isObfuscateMediaFiles()
	{
		return obfuscateMediaFiles;
	}

	/**
	 * @param obfuscateMediaFiles
	 *            the obfuscateMediaFiles to set
	 */
	public void setObfuscateMediaFiles(boolean obfuscateMediaFiles)
	{
		this.obfuscateMediaFiles = obfuscateMediaFiles;
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
	 * @return the backButtonDescription
	 */
	public String getBackButtonDescription()
	{
		return backButtonDescription;
	}

	/**
	 * @param backButtonDescription the backButtonDescription to set
	 */
	public void setBackButtonDescription(String backButtonDescription)
	{
		this.backButtonDescription = backButtonDescription;
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
	 * @return the cancelButtonDescription
	 */
	public String getCancelButtonDescription()
	{
		return cancelButtonDescription;
	}

	/**
	 * @param cancelButtonDescription the cancelButtonDescription to set
	 */
	public void setCancelButtonDescription(String cancelButtonDescription)
	{
		this.cancelButtonDescription = cancelButtonDescription;
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
	 * @return the forwardButtonDescription
	 */
	public String getForwardButtonDescription()
	{
		return forwardButtonDescription;
	}

	/**
	 * @param forwardButtonDescription the forwardButtonDescription to set
	 */
	public void setForwardButtonDescription(String forwardButtonDescription)
	{
		this.forwardButtonDescription = forwardButtonDescription;
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
		List<LocationField> locFields = new ArrayList<LocationField>();
		for(Field f : fields)
			if(f instanceof LocationField)
				locFields.add((LocationField) f);
		return locFields;
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
	 * @return the skipOnBack
	 */
	public boolean isSkipOnBack()
	{
		return skipOnBack;
	}

	/**
	 * @param skipOnBack the skipOnBack to set
	 */
	public void setSkipOnBack(boolean skipOnBack)
	{
		this.skipOnBack = skipOnBack;
	}

	/**
	 * @param next the next to set
	 * @throws IllegalArgumentException	when the nextStr is not recognised
	 */
	public void setNext(String nextStr) throws IllegalArgumentException
	{
		if(nextStr == null)
			return; //default next will be used
		if(nextStr.startsWith("_"))
			nextStr = nextStr.substring(1); //Strip off leading '_'
		nextStr = nextStr.toUpperCase(); //Make upper case
		try
		{
			this.next = Next.valueOf(nextStr);
		}
		catch(IllegalArgumentException iae)
		{
			if(V1X_NEXT_LOOP.equals(nextStr))
				this.next = Next.LOOPFORM;
			else if(V1X_NEXT_EXIT.equals(nextStr))
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
	 * @return the position within the project
	 */
	public int getPosition()
	{
		return position;
	}

	/**
	 * @return the producesRecords
	 */
	public boolean isProducesRecords()
	{
		if(producesRecords)
			getSchema(); // make sure getSchema() is at least called once
		return producesRecords;
	}
	
	public void initialiseStorage()
	{
		getSchema(); //this will also trigger all Columns to be created/initialised
	}
	
	/**
	 * The returned Schema object will contain all columns defined by fields in the form, plus the implicitly added
	 * columns (StartTime & DeviceID, which together are used as the primary key, and the optional EndTime). However,
	 * those implicit columns are only added if at least 1 user-defined field has a column. If there are no user-defined
	 * fields with columns then no implicit columns are added and then the whole schema is pointless, therefore in that
	 * case this method will return null instead of a columnless Schema object and the {@link #producesRecords} variable
	 * will be set to {@code false}. 
	 * 
	 * @return
	 */
	public Schema getSchema()
	{
		if(!producesRecords)
			return null;
		if(schema == null)
		{	
			// Generate columns for user-defined fields:
			List<Column<?>> userDefinedColumns = new ArrayList<Column<?>>();
			for(Field f : fields)
				/*	Important: do *NOT* check noColumn here and do *NOT* replace the call
				 *  to Field#addColumnTo(List<Column<?>>) by a call to Field#getColumn()! */
				f.addColumnTo(userDefinedColumns);
	
			// Check if there are user-defined columns at all, if not we don't need to generate a schema at all...
			if(userDefinedColumns.isEmpty())
			{
				producesRecords = false; // this will avoid that we try to generate a schema again
				// this.schema stays null
			}
			else
			{	
				// Create new Schema:
				schema = new Schema(SapelliCollectorClient.GetSchemaID(this), // combination of project hash and form index
									project.getName() +
									(project.getVariant() != null ? '_' + project.getVariant() : "") +
									"_v" + project.getVersion() +
									":" + id /* = form "name"*/);
				
				/* Add implicit columns
				 * 	StartTime & DeviceID together form the primary key of our records.
				 * 	These columns are implicitly added, together with EndTime if the
				 * 	appropriate attribute was set, *BUT* only if there is at least one
				 * 	user-defined field _with_ a column.
				 */
				// StartTime column:
				schema.addColumn(COLUMN_TIMESTAMP_START);
				// EndTime column:
				if(storeEndTime)
					schema.addColumn(COLUMN_TIMESTAMP_END);
				// Device ID column:
				schema.addColumn(COLUMN_DEVICE_ID);
				// Add primary key on StartTime & DeviceID:
				schema.addIndex(new Index(COLUMN_TIMESTAMP_START.getName() + "_" + COLUMN_DEVICE_ID.getName(), true, COLUMN_TIMESTAMP_START, COLUMN_DEVICE_ID), true);
				
				// Add user-defined columns
				schema.addColumns(userDefinedColumns);
				
				// Seal the schema:
				schema.seal();
			}
		}
		return schema;
	}
	
	/**
	 * Returns the column associated with the given field.
	 * 
	 * @param field
	 * @return the (non-virtual) column for the given field, or null in case the field has no column or the schema has not been initialised yet(!)
	 */
	public Column<?> getColumnFor(Field field)
	{
		if(!field.isNoColumn() && schema != null)
			return schema.getColumn(field.getID(), false);
		else
			return null;
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
			throw new IllegalArgumentException("The provided schema is not compatible with this form!");
	}*/

	public Record newRecord(long deviceID)
	{
		if(isProducesRecords())
		{
			Record record = getSchema().createRecord();
	
			// Set current time as start timestamp
			COLUMN_TIMESTAMP_START.storeValue(record, TimeStamp.now());
	
			// Set deviceID
			COLUMN_DEVICE_ID.storeValue(record, deviceID);
	
			return record;
		}
		else
			return null;
	}
	
	public TimeStamp getStartTime(Record record)
	{
		return getStartTime(record, false);
	}
	
	public TimeStamp getStartTime(Record record, boolean asStoredBinary)
	{
		if(asStoredBinary)
			return COLUMN_TIMESTAMP_START.retrieveValueAsStoredBinary(record);
		else
			return COLUMN_TIMESTAMP_START.retrieveValue(record);
	}
	
	public TimeStamp getEndTime(Record record)
	{
		if(isStoreEndTime())
			return COLUMN_TIMESTAMP_END.retrieveValue(record);
		else
			return null;
	}
	
	public long getDeviceID(Record record)
	{
		return COLUMN_DEVICE_ID.retrieveValue(record);
	}
	
	public void finish(Record record)
	{
		if(storeEndTime)
			// Set current time as end timestamp
			COLUMN_TIMESTAMP_END.storeValue(record, TimeStamp.now());
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
