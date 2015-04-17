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
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.Mode;
import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * @author mstevens
 *
 */
public abstract class Field extends JumpSource
{
	
	//Statics----------------------------------------------
	static public final int MAX_ID_LENGTH = Form.MAX_ID_LENGTH;
	
	//Defaults:
	static public final boolean DEFAULT_SHOW_BACK = true;
	static public final boolean DEFAULT_SHOW_CANCEL = true;
	static public final boolean DEFAULT_SHOW_FORWARD = true;
	static public final boolean DEFAULT_ENABLED = true;
	static public final boolean DEFAULT_SKIP_ON_BACK = false;
	static public final boolean DEFAULT_SHOW_ON_CREATE = true;
	static public final boolean DEFAULT_SHOW_ON_EDIT = true;
	static public final boolean DEFAULT_OPTIONAL = false;
	static public final boolean DEFAULT_NO_COLUMN = false;
	static public final boolean DEFAULT_EDITABLE = true;
	static public final String DEFAULT_BACKGROUND_COLOR = "#FFFFFF"; //white

	/**
	 * Returns the (trimmed) id to use or throws a NullPointerException if the id null, empty or white-space
	 * 
	 * @param parsedID
	 * @return the id to use (will never be null)
	 * @throws NullPointerException when the given ID is null or empty (possibly after trimming)
	 * @throws IllegalArgumentException when the given ID is too long (even after trimming), max length is {@value MAX_ID_LENGTH} characters.
	 */
	static private String GetID(String parsedID) throws NullPointerException, IllegalArgumentException
	{
		String trimmedID; // assigned below
		if(parsedID == null || (trimmedID = parsedID.trim()).isEmpty()) // don't sanitise here!
			throw new NullPointerException("Field ID cannot be null, empty or consist only of white-space.");
		if(trimmedID.length() > MAX_ID_LENGTH)
			throw new IllegalArgumentException("Field ID \"" + parsedID + "\" is too long (max length: " + MAX_ID_LENGTH + ").");
		return trimmedID;
	}
	
	/**
	 * Returns the id to use, possibly generated from a caption or the field index within the form
	 * 
	 * @param parsedId
	 * @param form
	 * @param captionPrefix
	 * @param parsedCaption
	 * @return the id to use (will never be null)
	 * @throws IllegalArgumentException when the given ID is too long (even after trimming), max length is {@value MAX_ID_LENGTH} characters.
	 */
	static protected String GetID(String parsedId, Form form, String captionPrefix, String parsedCaption) throws IllegalArgumentException
	{
		// Try using the given ID:
		try
		{
			return GetID(parsedId); // throws IAE when the ID is too long (not caught here!)
		}
		catch(NullPointerException npe) { /* ignore */ }
		// Generate an ID instead (+ call GetID(String) for length check):
		return GetID(Column.SanitiseName(captionPrefix + (parsedCaption == null || parsedCaption.trim().isEmpty() ? form.getNumberOfFields(true) : parsedCaption.trim())));
		//	Note: this generated id will not occur in the XML so we can already sanitise it for use as a Column-name.
	}
	
	/**
	 * Returns the (parsed) caption if it isn't null or throws a NullPointerException if it is.
	 * Only to be used on Fields on which a caption is required!
	 * Note that empty-String and all-whitespace captions are accepted.
	 * 
	 * @param parsedCaption
	 * @return
	 * @throws NullPointerException
	 */
	static protected String CheckCaption(String parsedCaption) throws NullPointerException
	{
		if(parsedCaption == null)
			throw new NullPointerException("Caption cannot be null.");
		return parsedCaption; // don't sanitise here!
	}
	
	//Dynamics---------------------------------------------
	public final String id;
	public final Form form;
	protected Page page = null;
	protected final String caption;
	public final Description description;
	protected boolean enabled = DEFAULT_ENABLED;
	protected boolean skipOnBack = DEFAULT_SKIP_ON_BACK;
	protected boolean showOnCreate = DEFAULT_SHOW_ON_CREATE;
	protected boolean showOnEdit = DEFAULT_SHOW_ON_EDIT;
	protected boolean optional = DEFAULT_OPTIONAL;
	protected boolean noColumn = DEFAULT_NO_COLUMN;
	protected boolean editable = DEFAULT_EDITABLE;
	protected String backgroundColor = DEFAULT_BACKGROUND_COLOR;
	
	// Buttons:
	private boolean[][] showControlByMode;
	
	/**
	 * @param form the form the field belongs to
	 * @param id the id of the field, should not be null
	 */
	public Field(Form form, String id) throws NullPointerException
	{
		this(form, id, null); // caption is null
	}
	
	/**
	 * @param form the form the field belongs to
	 * @param id the id of the field, should not be null, or empty after trimming
	 * @param caption the caption of the field, may be null
	 * @throws NullPointerException when id is null or empty after trimming
	 */
	public Field(Form form, String id, String caption) throws NullPointerException
	{
		this.id = GetID(id); // will trim the id and throw an NPE when the id is null, empty or whitespace
		this.form = form;
		this.caption = caption; // may be null!
		this.description = new Description();
		
		// Construct a 2-dimensional boolean array (Control.Type * FormMode):
		this.showControlByMode = new boolean[Control.Type.values().length][];
		for(Control.Type controlType : Control.Type.values())
		{
			showControlByMode[controlType.ordinal()] = new boolean[Controller.Mode.values().length];
			for(Mode mode : Controller.Mode.values())
			{
				boolean defaultShown = true;
				switch(controlType)
				{
					case Back: defaultShown = DEFAULT_SHOW_BACK;
						break;
					case Cancel: defaultShown = DEFAULT_SHOW_CANCEL;
						break;
					case Forward: defaultShown = DEFAULT_SHOW_FORWARD;
						break;
				}
				showControlByMode[controlType.ordinal()][mode.ordinal()] = defaultShown;
			}
		}
	}
	
	/**
	 * @return the page (will be null if this field is not contained by a page)
	 */
	public Page getPage()
	{
		return page;
	}

	/**
	 * @return whether or not the field is part of a page
	 */
	public boolean isOnPage()
	{
		return page != null;
	}
	
	/**
	 * @param page the page to set
	 */
	public void setPage(Page page)
	{
		this.page = page;
	}

	/**
	 * @return the id
	 */
	public final String getID()
	{
		return id;
	}
	
	/**
	 * @return the label
	 */
	public String getCaption()
	{
		return caption;
	}
	
	/**
	 * @return whether or not there is a non-null caption ("" is a valid caption)
	 */
	public boolean hasCaption()
	{
		return getCaption() != null; // don't change getCaption() to caption (needed for MultiListField)
	}
	
	/**
	 * @return the noColumn
	 */
	public boolean isNoColumn()
	{
		return noColumn;
	}

	/**
	 * @param noColumn the noColumn to set
	 */
	public void setNoColumn(boolean noColumn)
	{
		this.noColumn = noColumn;
	}
	
	/**
	 * @param optional the optional to set
	 */
	public void setOptional(boolean optional)
	{
		this.optional = optional;
	}

	public boolean isOptional()
	{
		return optional;
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
	 * By default fields that are contained within a Page cannot jump away from the page
	 * Some subclasses (i.e. ButtonField) will override this. 
	 * 
	 * @return
	 */
	public boolean canJumpFromPage()
	{
		return false;
	}
	
	/**
	 * @return the showOnCreate
	 */
	public boolean isShowOnCreate()
	{
		return showOnCreate;
	}

	/**
	 * @param showOnCreate the showOnCreate to set
	 */
	public void setShowOnCreate(boolean showOnCreate)
	{
		this.showOnCreate = showOnCreate;
	}

	/**
	 * @return the showOnEdit
	 */
	public boolean isShowOnEdit()
	{
		return showOnEdit;
	}

	/**
	 * @param showOnEdit the showOnEdit to set
	 */
	public void setShowOnEdit(boolean showOnEdit)
	{
		this.showOnEdit = showOnEdit;
	}
	
	/**
	 * @return the enabled
	 */
	public boolean isEnabled()
	{
		return enabled;
	}
	
	/**
	 * @return the editable
	 */
	public boolean isEditable()
	{
		return editable;
	}

	/**
	 * @param editable the editable to set
	 */
	public void setEditable(boolean editable)
	{
		this.editable = editable;
	}

	/**
	 * @return the backgroundColor
	 */
	public String getBackgroundColor()
	{
		return backgroundColor;
	}

	/**
	 * @param backgroundColor the backgroundColor to set
	 */
	public void setBackgroundColor(String backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}

	/**
	 * @param controlType
	 * @param formMode
	 * @return whether of the the giving control is allowed to be show for this field when in the given formMode
	 */
	public boolean isControlAllowedToBeShown(Control.Type controlType, Mode formMode)
	{
		return showControlByMode[controlType.ordinal()][formMode.ordinal()];
	}
	
	/**
	 * @param controlType
	 * @param formMode
	 * @param show
	 */
	public void setShowControlOnMode(Control.Type controlType, Mode formMode, boolean show)
	{
		showControlByMode[controlType.ordinal()][formMode.ordinal()] = show;
	}
	
	/**
	 * @param controlType
	 * @param show
	 */
	public void setShowControl(Control.Type controlType, boolean show)
	{
		for(Mode mode : Controller.Mode.values())
			showControlByMode[controlType.ordinal()][mode.ordinal()] = show;
	}

	/**
	 * @param showBack the showBack to set
	 */
	public void setShowBack(boolean showBack)
	{
		setShowControl(Control.Type.Back, showBack);
	}

	/**
	 * @param showCancel the showCancel to set
	 */
	public void setShowCancel(boolean showCancel)
	{
		setShowControl(Control.Type.Cancel, showCancel);
	}

	/**
	 * @param showForward the showForward to set
	 */
	public void setShowForward(boolean showForward)
	{
		setShowControl(Control.Type.Forward, showForward);
	}

	public Column<?> getColumn()
	{
		// Deal with root...
		if(!isRoot())
			return getRoot().getColumn();
		
		// Check noColumn...
		if(noColumn)
			return null;
		
		// Check if the form (or rather its schema) already has a column for this field:
		Column<?> schemaCol = form.getColumnFor(this);
		
		if(schemaCol == null)
		{	// there's no column yet, most likely the method was called as part of the schema initialisation process, so we create a column for this field
			
			//	Sanitise field id to remove characters that are illegal in column (& XML) names:
			String colName = Column.SanitiseName(id);
			//	Warn about illegal chars:
			if(!colName.equals(id))
				form.addWarning("Field ID \"" + id + "\" contains 1 or more characters that are illegal in a data column name. The column corresponding to this field will be named \"" + colName + "\" instead.");
			// Create column:
			Column<?> createdCol = createColumn(colName);
			//	Check if createColumn() implementation respected the naming contract:
			if(createdCol != null && !createdCol.getName().equals(colName))
				throw new IllegalStateException("Unexpected column name (got: " + createdCol.getName() + "; expected: " + colName + ") , this indicates an incorrect createColumn(String) implementation.");
			//	Return just created column...
			return createdCol;
		}
		else
			return schemaCol; // return previously created column
	}
	
	/**
	 * Method that asks the Field to add its column to the provided list.
	 * 
	 * Should be overridden in composite Fields like {@link Page}!
	 * 
	 * @param columns
	 */
	protected void addColumnTo(List<Column<?>> columns)
	{
		CollectionUtils.addIgnoreNull(columns, getColumn());
	}
	
	/**
	 * Important:<br/>
	 * 	- it is assumed this method is *only* called if noColumn=false;<br/>
	 *	- it is assumed that the provided name String is used (unchanged!) as the column's name (instead of just using the field's id as before)
	 * 
	 * @param name the column name to use
	 * @return a column instance
	 */
	protected abstract Column<?> createColumn(String name);
	
	/**
	 * Meant to be overridden in (some) subclasses
	 * 
	 * @return the root Field of this Field
	 */
	public Field getRoot()
	{
		return this;
	}
	
	/**
	 * Meant to be overridden in (some) subclasses
	 * 
	 * @return whether or not this is a root Field
	 */
	public boolean isRoot()
	{
		return this == getRoot();
	}
	
	/**
	 * @return the form
	 */
	public Form getForm()
	{
		return form;
	}

	/**
	 * To be overridden by Fields that use additional files (images, sounds, etc.) that are stored with the project,
	 * all overrides *must* include: super.addFiles(filesSet, fileStorageProvider);
	 * 
	 * @param filesSet set to add files to
	 * @param fileStorageProvider to resolve relative paths
	 * @return
	 */
	public void addFiles(Set<File> filesSet, FileStorageProvider fileStorageProvider)
	{
		// Audio description:
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectSoundFile(form.project, description.getAudioRelativePath()));
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof Field)
		{
			Field that = (Field) obj;
			return	super.equals(that) && // JumpSource#equals(Object)
					this.id.equals(that.id) &&
					this.form.toString().equals(that.form.toString()) && // DO NOT INCLUDE form ITSELF HERE (otherwise we create an endless loop!)
					this.form.project.toString().equals(that.form.project.toString()) && // DO NOT INCLUDE form.project ITSELF HERE (otherwise we create an endless loop!)
					(this.caption != null ? caption.equals(that.caption) : that.caption == null) &&
					this.description.equals(that.description) &&
					this.enabled == that.enabled &&
					this.skipOnBack == that.skipOnBack &&
					this.showOnCreate == that.showOnCreate &&
					this.showOnEdit == that.showOnEdit &&
					this.optional == that.optional &&
					this.noColumn == that.noColumn &&
					this.editable == that.editable &&
					this.backgroundColor.equals(that.backgroundColor);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // JumpSource#hashCode()
		hash = 31 * hash + id.hashCode();
		hash = 31 * hash + form.toString().hashCode(); // DO NOT INCLUDE form ITSELF HERE (otherwise we create an endless loop!)
		hash = 31 * hash + form.project.toString().hashCode(); // DO NOT INCLUDE form.project ITSELF HERE (otherwise we create an endless loop!)
		hash = 31 * hash + (caption == null ? 0 : caption.hashCode());
		hash = 31 * hash + description.hashCode();
		hash = 31 * hash + (enabled ? 0 : 1);
		hash = 31 * hash + (skipOnBack ? 0 : 1);
		hash = 31 * hash + (showOnCreate ? 0 : 1);
		hash = 31 * hash + (showOnEdit ? 0 : 1);
		hash = 31 * hash + (optional ? 0 : 1);
		hash = 31 * hash + (noColumn ? 0 : 1);
		hash = 31 * hash + (editable ? 0 : 1);
		hash = 31 * hash + backgroundColor.hashCode();
		return hash;
	}
	
	/**
	 * Called to signal that the user is entering the field. This allows any required initialisation behaviour to be carried out.
	 * The returned boolean indicates whether or not a UI update is required after entering the field.
	 * 
	 *  This method uses double-dispatch: the actual Field-type-specific behaviour will be defined in the class implementing the Controller interface.
	 * 
	 * @param visitor a FieldVisitor (usually a Controller)
	 * @param arguments arguments passed from previous field (should never be null, but will often be the FieldParameters.EMPTY object)
	 * @param withPage whether or not the field is entered because the page containing it entered (true) or because it is entered on its own (false)
	 * @return whether or not a UI update is required after entering the field)
	 */
	public abstract boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage);
	
	/**
	 * Returns a FieldUI object to represent this Field.
	 * 
	 * This method uses double-dispatch: the actual FieldUI object will be instantiated by the class implementing the CollectorUI interface.
	 * 
	 * @param collectorUI
	 * @return
	 */
	public abstract <V, UI extends CollectorUI<V, UI>> FieldUI<? extends Field, V, UI> createUI(UI collectorUI);
}
