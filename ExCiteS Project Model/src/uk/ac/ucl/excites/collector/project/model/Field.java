package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.storage.model.Column;

/**
 * @author mstevens
 *
 */
public abstract class Field
{
	
	//Statics----------------------------------------------
	static public enum Optionalness
	{
	    ALWAYS,
	    NOT_IF_REACHED,
	    NEVER
	}
	
	//Defaults:
	public static final boolean DEFAULT_SHOW_BACK = true;
	public static final boolean DEFAULT_SHOW_CANCEL = true;
	public static final boolean DEFAULT_SHOW_FORWARD = true;
	static public final boolean DEFAULT_ENABLED = true;
	static public final Optionalness DEFAULT_OPTIONAL = Optionalness.NOT_IF_REACHED;
	static public final boolean DEFAULT_NO_COLUMN = false;
	
	//Dynamics---------------------------------------------
	protected String id;
	protected Form form;
	protected Field jump;
	protected Column<?> column;
	protected boolean enabled = DEFAULT_ENABLED;
	protected Optionalness optional = DEFAULT_OPTIONAL;
	protected boolean noColumn = DEFAULT_NO_COLUMN;
	// Buttons:
	private boolean showBack = DEFAULT_SHOW_BACK;
	private boolean showCancel = DEFAULT_SHOW_CANCEL;
	private boolean showForward = DEFAULT_SHOW_FORWARD;
	
	public Field(Form form, String id)
	{
		if(id == null || id.isEmpty())
			throw new NullPointerException("ID cannot be null or empty.");
		this.id = id.trim();
		this.form = form;
	}
	
	/**
	 * @return the id
	 */
	public String getID()
	{
		return id;
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

	public void setJump(Field target)
	{
		this.jump = target;
	}
	
	public Field getJump()
	{
		return jump;
	}
	
	/**
	 * @return the enabled
	 */
	public boolean isEnabled()
	{
		return enabled;
	}
	
	public void disable()
	{
		enabled = false;
	}
	
	public void enable()
	{
		enabled = true;
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

	public Column<?> getColumn()
	{
		if(!noColumn && this.column == null)
			column = createColumn();
		return column;
	}
	
	/**
	 * Returns a new Column object capable of storing values for this field
	 * It is assumed that the field.id is used as the column name.
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
		return true;
	}
	
	/**
	 * @return the form
	 */
	public Form getForm()
	{
		return form;
	}

	public abstract void setIn(CollectorUI ui);
	
}
