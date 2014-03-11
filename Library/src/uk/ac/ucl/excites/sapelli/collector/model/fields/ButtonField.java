/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.DateTimeColumn;
import uk.ac.ucl.excites.sapelli.util.StringUtils;

/**
 * A Field that represents an on-screen button, it can optionally have either Boolean- or DateTime column.
 * 
 * @author mstevens
 */
public class ButtonField extends Field
{
	
	// Statics --------------------------------------------
	static public enum ButtonColumn
	{
		NONE,
		BOOLEAN,
		DATETIME
	}
	
	static public final String ID_PREFIX = "btn";
	static public final ButtonColumn DEFAULT_COLUMN = ButtonColumn.NONE;
	
	// Dynamics -------------------------------------------
	private ButtonColumn column;
	
	/**
	 * @param form
	 * @param label
	 */
	public ButtonField(Form form, String label)
	{
		this(form, null, label);
	}
	
	/**
	 * @param form
	 * @param id
	 * @param label
	 */
	public ButtonField(Form form, String id, String label)
	{	
		super(form, (id == null || id.isEmpty() ? ID_PREFIX + (label.trim().isEmpty() ? form.getFields().size() : StringUtils.replaceWhitespace(label.trim(), "_")) : id), label);
		setColumn(DEFAULT_COLUMN);
	}

	public void setColumn(String column) throws IllegalArgumentException
	{
		setColumn(ButtonColumn.valueOf(column.toUpperCase()));
	}
	
	public void setColumn(ButtonColumn column)
	{
		this.column = column;
		this.noColumn = (this.column == ButtonColumn.NONE); 
	}
	
	public void setNoColumn(boolean noColumn)
	{
		throw new UnsupportedOperationException("setNoColumn() is unsupported on ButtonFields, use setColumn() instead.");
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		switch(column)
		{
			case BOOLEAN : return new BooleanColumn(id, optional != Optionalness.NEVER);
			case DATETIME : return DateTimeColumn.Century21NoMS(id, optional != Optionalness.NEVER);
			/* case NONE */ default : return null;
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#enter(uk.ac.ucl.excites.collector.project.ui.Controller)
	 */
	@Override
	public boolean enter(Controller controller)
	{
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites.collector.project.ui.CollectorUI)
	 */
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return collectorUI.createButtonFieldUI(this);
	}

}
