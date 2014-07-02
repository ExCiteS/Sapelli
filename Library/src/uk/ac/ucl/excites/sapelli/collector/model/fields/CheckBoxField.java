/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.CheckBoxUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;

/**
 * @author Julia
 * 
 */
public class CheckBoxField extends Field
{

	static public final String ID_PREFIX = "chbx";
	
	static public final boolean DEFAULT_INITIAL_VALUE = false; // not ticked by default
	
	private boolean initialValue;

	/**
	 * @param form
	 * @param id
	 * @param caption
	 */
	public CheckBoxField(Form form, String id, String caption)
	{
		super(	form,
				(id == null || id.isEmpty() ? captionToID(ID_PREFIX, form, caption) : id),
				caption);
		this.initialValue = DEFAULT_INITIAL_VALUE;
	}

	/**
	 * @return the initialValue
	 */
	public boolean getInitialValue()
	{
		return initialValue;
	}
	
	/**
	 * @param initialValue the initialValue to set
	 */
	public void setInitialValue(boolean initialValue)
	{
		this.initialValue = initialValue;
	}

	@Override
	public BooleanColumn getColumn()
	{
		return (BooleanColumn) super.getColumn();
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		return new BooleanColumn(id, optional != Optionalness.NEVER);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, boolean)
	 */
	@Override
	public boolean enter(Controller controller, FieldParameters arguments, boolean withPage)
	{
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> CheckBoxUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createCheckBoxFieldUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof CheckBoxField)
		{
			CheckBoxField that = (CheckBoxField) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.initialValue == that.initialValue;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + (initialValue ? 0 : 1);
		return hash;
	}

}
