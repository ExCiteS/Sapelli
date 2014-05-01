package uk.ac.ucl.excites.sapelli.collector.model;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.FormMode;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.Control;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * @author mstevens
 *
 */
/**
 * @author mstevens
 *
 */
public abstract class Field extends JumpSource
{
	
	//Statics----------------------------------------------
	static public enum Optionalness
	{
	    ALWAYS,
	    NOT_IF_REACHED,
	    NEVER
	}
	
	static public String captionToID(String prefix, Form form, String caption)
	{
		// TODO remove chars that are illegal in XML tag names
		return prefix + (caption.trim().isEmpty() ? form.getFields().size() : StringUtils.replaceWhitespace(caption.trim(), "_").replace(":", ""));
	}
	
	//Defaults:
	static public final boolean DEFAULT_SHOW_BACK = true;
	static public final boolean DEFAULT_SHOW_CANCEL = true;
	static public final boolean DEFAULT_SHOW_FORWARD = true;
	static public final boolean DEFAULT_ENABLED = true;
	static public final boolean DEFAULT_SKIP_ON_BACK = false;
	static public final boolean DEFAULT_SHOW_ON_CREATE = true;
	static public final boolean DEFAULT_SHOW_ON_EDIT = true;
	static public final Optionalness DEFAULT_OPTIONAL = Optionalness.NOT_IF_REACHED;
	static public final boolean DEFAULT_NO_COLUMN = false;
	static public final boolean DEFAULT_EDITABLE = true;
	static public final String DEFAULT_BACKGROUND_COLOR = "#FFFFFF"; //white
	
	
	//Dynamics---------------------------------------------
	protected String id;
	protected String caption;
	protected Form form;
	protected boolean enabled = DEFAULT_ENABLED;
	protected boolean skipOnBack = DEFAULT_SKIP_ON_BACK;
	protected boolean showOnCreate = DEFAULT_SHOW_ON_CREATE;
	protected boolean showOnEdit = DEFAULT_SHOW_ON_EDIT;
	protected Optionalness optional = DEFAULT_OPTIONAL;
	protected boolean noColumn = DEFAULT_NO_COLUMN;
	protected boolean editable = DEFAULT_EDITABLE;
	protected String backgroundColor = DEFAULT_BACKGROUND_COLOR;
	
	// Buttons:
	private boolean[][] showControlByMode;
	
	/**
	 * @param form the form the field belongs to
	 * @param id the id of the field, should not be null
	 */
	public Field(Form form, String id)
	{
		this(form, id, id); // use id as default caption
	}
	
	/**
	 * @param form the form the field belongs to
	 * @param id the id of the field, should not be null
	 * @param caption the caption of the field, may be null (in which case the id is used as the caption)
	 */
	public Field(Form form, String id, String caption)
	{
		//TODO check if id is valid column name
		if(id == null || id.trim().isEmpty())
			throw new NullPointerException("Top-level field ID cannot be null or empty.");
		this.form = form;
		this.id = id.trim();
		this.caption = caption == null ? this.id : caption;
		
		// Construct a 2-dimensional boolean array (Controls x FormMode):
		this.showControlByMode = new boolean[ControlsUI.Control.values().length][];
		for(Control control : ControlsUI.Control.values())
		{
			showControlByMode[control.ordinal()] = new boolean[Controller.FormMode.values().length];
			for(FormMode mode : Controller.FormMode.values())
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
	 * @return the optional
	 */
	public Optionalness getOptional()
	{
		return optional;
	}

	/**
	 * @param optional the optional to set
	 */
	public void setOptional(Optionalness optionalness)
	{
		this.optional = optionalness;
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
	public boolean isControlAllowedToBeShown(Control control, FormMode formMode)
	{
		return showControlByMode[control.ordinal()][formMode.ordinal()];
	}
	
	/**
	 * @param control
	 * @param formMode
	 * @param show
	 */
	public void setShowControlOnMode(Control control, FormMode formMode, boolean show)
	{
		showControlByMode[control.ordinal()][formMode.ordinal()] = show;
	}
	
	/**
	 * @param control
	 * @param show
	 */
	public void setShowControl(Control control, boolean show)
	{
		for(FormMode mode : Controller.FormMode.values())
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
			/* the form's schema is not yet initialised, and most likely this method was called as
			 * part of the initialisation process, so return a newly created column for this field: */
			return createColumn();
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
	 * 	- it is assumed that the field.id will be used as the column's name.
	 * 
	 * @return
	 */
	protected abstract Column<?> createColumn();
	
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
	 * @param project
	 * @return
	 */
	public List<File> getFiles(Project project)
	{
		return null;
	}
	
	/**
	 * Called to signal that the user is entering the field. This allows any required initialisation behaviour to be carried out.
	 * The returned boolean indicates whether or not a UI update is required after entering the field.
	 * 
	 *  This method uses double-dispatch: the actual Field-type-specific behaviour will be defined in the class implementing the Controller interface.
	 * 
	 * @param controller
	 * @param arguments arguments passed from previous field (should never be null, but will often be the FieldParameters.EMPTY object)
	 * @param withPage whether or not the field is entered because the page containing it entered (true) or because it is entered on its own (false)
	 * @return whether or not a UI update is required after entering the field)
	 */
	public abstract boolean enter(Controller controller, FieldParameters arguments, boolean withPage);
	
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
