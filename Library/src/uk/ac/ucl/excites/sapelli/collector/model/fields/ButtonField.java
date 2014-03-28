/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.ButtonUI;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.DateTimeColumn;

/**
 * A Field that represents an on-screen button, it can optionally have either Boolean- or DateTime column.
 * 
 * @author mstevens
 */
/**
 * @author mstevens
 *
 */
public class ButtonField extends Field
{
	
	// Statics --------------------------------------------
	static public enum ButtonColumnType
	{
		NONE,
		BOOLEAN,
		DATETIME
	}
	
	static public final String ID_PREFIX = "btn";
	static public final ButtonColumnType DEFAULT_COLUMN = ButtonColumnType.NONE;
	
	// Dynamics -------------------------------------------
	private ButtonColumnType columnType;
	
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
		setColumnType(DEFAULT_COLUMN);
	}

	public void setColumnType(String column) throws IllegalArgumentException
	{
		setColumnType(ButtonColumnType.valueOf(column.toUpperCase()));
	}
	
	public void setColumnType(ButtonColumnType columnType)
	{
		this.columnType = columnType;
		this.noColumn = (this.columnType == ButtonColumnType.NONE); 
	}
	
	/**
	 * @return the columnType
	 */
	public ButtonColumnType getColumnType()
	{
		return columnType;
	}

	public void setNoColumn(boolean noColumn)
	{
		this.noColumn = noColumn;
		if(noColumn)
			this.columnType = ButtonColumnType.NONE;
		else
			throw new UnsupportedOperationException("setNoColumn(false) is unsupported on ButtonFields, use setColumn() instead.");
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		switch(columnType)
		{
			case BOOLEAN : return new BooleanColumn(id, optional != Optionalness.NEVER);
			case DATETIME : return DateTimeColumn.Century21NoMS(id, optional != Optionalness.NEVER);
			/* case NONE */ default : return null;
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, boolean)
	 */
	@Override
	public boolean enter(Controller controller, boolean withPage)
	{
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites.collector.project.ui.CollectorUI)
	 */
	@Override
	public <V> ButtonUI<V> createUI(CollectorUI<V> collectorUI)
	{
		return collectorUI.createButtonUI(this);
	}

}
