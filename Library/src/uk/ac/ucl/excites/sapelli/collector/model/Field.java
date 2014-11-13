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
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.Mode;
import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.Control;
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
	 * @return
	 * @throws NullPointerException
	 */
	static private String GetID(String parsedID) throws NullPointerException
	{
		if(parsedID == null || parsedID.trim().isEmpty())
			throw new NullPointerException("Top-level field ID cannot be null, empty or consist only of white-space.");
		return parsedID.trim(); // don't sanitise here!
	}
	
	/**
	 * Returns the id to use, possibly generated from a caption or the field index within the form
	 * 
	 * @param parsedId
	 * @param form
	 * @param captionPrefix
	 * @param parsedCaption
	 * @return the id to use (will never be null)
	 */
	static protected String GetID(String parsedId, Form form, String captionPrefix, String parsedCaption)
	{		
		if(parsedId != null && !parsedId.trim().isEmpty())
			return parsedId; // don't sanitise here!
		return Column.SanitiseName(captionPrefix + (parsedCaption == null || parsedCaption.trim().isEmpty() ? form.getFields().size() : parsedCaption.trim())); // this generated id will not occur in the XML so we can already sanitise
	}
	
	/**
	 * Returns the caption to use, possibly taken as the id
	 * 
	 * @param parsedId
	 * @param parsedCaption
	 * @return the caption to use (may be null!)
	 */
	static protected String GetCaption(String parsedId, String parsedCaption)
	{
		if(parsedCaption != null)
			return parsedCaption;
		return parsedId;
	}
	
	//Dynamics---------------------------------------------
	protected final String id;
	public final Form form;
	protected final String caption;
	protected String description;
	protected String descriptionAudioRelativePath;
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
		
		// Construct a 2-dimensional boolean array (Controls * FormMode):
		this.showControlByMode = new boolean[ControlsUI.Control.values().length][];
		for(Control control : ControlsUI.Control.values())
		{
			showControlByMode[control.ordinal()] = new boolean[Controller.Mode.values().length];
			for(Mode mode : Controller.Mode.values())
			{
				boolean defaultShown = true;
				switch(control)
				{
				case BACK: defaultShown = DEFAULT_SHOW_BACK;
					break;
				case CANCEL: defaultShown = DEFAULT_SHOW_CANCEL;
					break;
				case FORWARD: defaultShown = DEFAULT_SHOW_FORWARD;
					break;					
				}
				showControlByMode[control.ordinal()][mode.ordinal()] = defaultShown;
			}
		}
	}
	
	/**
	 * @return the id
	 */
	public String getID()
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
	 * @return the description
	 */
	public String getDescription()
	{
		return description;
	}

	/**
	 * @param description the description to set
	 */
	public void setDescription(String description)
	{
		this.description = description;
	}

	/**
	 * @return the descriptionAudioRelativePath
	 */
	public String getDescriptionAudioRelativePath()
	{
		return descriptionAudioRelativePath;
	}

	/**
	 * @param descriptionAudioRelativePath the descriptionAudioRelativePath to set
	 */
	public void setDescriptionAudioRelativePath(String descriptionAudioRelativePath)
	{
		this.descriptionAudioRelativePath = descriptionAudioRelativePath;
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
	 * @param control
	 * @param formMode
	 * @return whether of the the giving control is allowed to be show for this field when in the given formMode
	 */
	public boolean isControlAllowedToBeShown(Control control, Mode formMode)
	{
		return showControlByMode[control.ordinal()][formMode.ordinal()];
	}
	
	/**
	 * @param control
	 * @param formMode
	 * @param show
	 */
	public void setShowControlOnMode(Control control, Mode formMode, boolean show)
	{
		showControlByMode[control.ordinal()][formMode.ordinal()] = show;
	}
	
	/**
	 * @param control
	 * @param show
	 */
	public void setShowControl(Control control, boolean show)
	{
		for(Mode mode : Controller.Mode.values())
			showControlByMode[control.ordinal()][mode.ordinal()] = show;
	}

	/**
	 * @param showBack the showBack to set
	 */
	public void setShowBack(boolean showBack)
	{
		setShowControl(Control.BACK, showBack);
	}

	/**
	 * @param showCancel the showCancel to set
	 */
	public void setShowCancel(boolean showCancel)
	{
		setShowControl(Control.CANCEL, showCancel);
	}

	/**
	 * @param showForward the showForward to set
	 */
	public void setShowForward(boolean showForward)
	{
		setShowControl(Control.FORWARD, showForward);
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
	 * @return
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
	 * To be overridden by Fields that use files (images, sounds, etc.) that are stored with the project
	 * 
	 * @param fileStorageProvider to resolve relative paths
	 * @return
	 */
	public List<File> getFiles(FileStorageProvider fileStorageProvider)
	{
		return Collections.<File> emptyList();
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
					this.form.getProject().toString().equals(that.form.getProject().toString()) && // DO NOT INCLUDE form.project ITSELF HERE (otherwise we create an endless loop!)
					(this.caption != null ? caption.equals(that.caption) : that.caption == null) &&
					(this.description != null ? this.description.equals(that.description) : that.description == null) &&
					(this.descriptionAudioRelativePath != null ? this.descriptionAudioRelativePath.equals(that.descriptionAudioRelativePath) : that.descriptionAudioRelativePath == null) &&
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
		hash = 31 * hash + form.getProject().toString().hashCode(); // DO NOT INCLUDE form.project ITSELF HERE (otherwise we create an endless loop!)
		hash = 31 * hash + (caption == null ? 0 : caption.hashCode());
		hash = 31 * hash + (description == null ? 0 : description.hashCode());
		hash = 31 * hash + (descriptionAudioRelativePath == null ? 0 : descriptionAudioRelativePath.hashCode());
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
	public abstract <V, UI extends CollectorUI<V, UI>> FieldUI<?, V, UI> createUI(UI collectorUI);
	
}
