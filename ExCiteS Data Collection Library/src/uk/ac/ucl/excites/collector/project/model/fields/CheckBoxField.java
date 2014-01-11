/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model.fields;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.project.ui.Controller;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.BooleanColumn;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;

/**
 * @author Julia
 * 
 */
public class CheckBoxField extends Field
{

	static public final String ID_PREFIX = "chbx";
	private String label;
	private boolean value;

	// Defaults
	public static final boolean DEFAULT_VALUE = false; // not ticked by default

	/**
	 * @param form
	 * @param id
	 * @parap label
	 */
	public CheckBoxField(Form form, String id, String label)
	{
		super(form, (id == null || id.isEmpty() ? ID_PREFIX + label.trim().replaceAll("\\s+", "_") : id));
		this.label = label;
		this.value = DEFAULT_VALUE;
	}

	/**
	 * @return the label
	 */
	public String getLabel()
	{
		return label;
	}

	/**
	 * @return the value
	 */
	public boolean getValue()
	{
		return value;
	}

	/**
	 * @param value
	 *            the value to set
	 */
	public void setValue(boolean value)
	{
		this.value = value;
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

	public void storeValue(Record record)
	{
		if(!isNoColumn())
			((BooleanColumn) form.getColumnFor(this)).storeValue(record, value);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#enter(uk.ac.ucl.excites .collector.project.ui.Controller)
	 */
	@Override
	public boolean enter(Controller controller)
	{
		return controller.enterCheckBoxField(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites .collector.project.ui.CollectorUI)
	 */
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return collectorUI.createCheckBoxFieldUI(this);
	}

}
